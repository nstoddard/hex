{-# LANGUAGE TupleSections, FlexibleContexts #-}

import Control.Monad
import Control.Applicative
import Control.Arrow hiding (loop)
import Data.List
import System.Exit
import Data.IORef
import Data.Array.IO
import Data.Int
import System.IO
import Data.Maybe
import System.Random
import Data.Ix
import qualified Data.Map as M
import Data.Char
import GHC.Exts
import Control.Exception
import System.IO.Error

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import RandomExtra
import ListExtra
import NumExtra
import IOExtra
import MiscExtra
import OldGUI
import Vect
import ConfigFormat

--TODO: calculate % of first-move wins

configFile = "config.cfg"


logFilePath state = getStrOpt (stConfig state) "logFilePath"
writeLogFiles state = getBoolOpt (stConfig state) "writeLogFiles"
logFile state = logFilePath state ++ "_winners.log"
logFile2 state = logFilePath state ++ "winners.log"
summaryFile state = logFilePath state ++ "summary.log"
hideCursor state = hideCursor_ (stConfig state)
hideCursor_ config = getBoolOpt config "hideCursor"
maxGames state = if getBoolOpt (stConfig state) "limitGames"
  then Just $ getIntOpt (stConfig state) "maxGames"
  else Nothing
allowAI_ config = getBoolOpt config "allowAI"

type LogFile = M.Map String LogEntry
data LogEntry = LogEntry Int Int [(String,Bool,Bool)] deriving (Read,Show)

writeLogFile state winner loser = writeLogFile_ state (sanitizeName winner) (sanitizeName loser)

sanitizeName name = case name' of
  [] -> "<no name>"
  (fst:rest) -> toUpper fst : map toLower rest
  where name' = filter isAlpha name

sanitizeLogEntry (LogEntry wins losses games) = LogEntry wins losses
  (map sanitizeGame games)

sanitizeGame (name, a, b) = (sanitizeName name, a, b)

{-

Player		win	loss	win %
adjacent	2	2	50.00%
random    1 0
random    1 0
test      0 1
test      0 1

random		0	4	0.00%
...

-}

showLogFile dat = ((show (totalGames dat) ++ " games played\n\n") ++) $
  ((pad "Player" ++ "\tWins\tLosses\tWin %\n") ++) $
  concatFor (M.toList dat) $ \(name, LogEntry wins losses games) ->
    pad name ++ "\t" ++ show wins ++ "\t" ++ show losses ++ "\t" ++
    showWinPercent wins losses ++ "\n" ++
    intercalate "\n" (map (showGame name) games {-$ f games-}) ++ "\n\n"
  where
    --f = map (id &&& length) . groupWith fst
    {-showGame (games, count) = show count ++ " games vs " ++ b ++ ": " ++
      show wins ++ " wins, " ++ show losses ++ " losses"
      where
        b = fst (head games)
        wins = countBy ((==True) . snd) games
        losses = countBy ((==False) . snd) games-}
    showGame firstPlayer (name, isWin,wentFirst) = pad name ++ "\t" ++
      --TODO: format this better
      show (if isWin then 1 else 0) ++ "\t" ++ show (if isWin then 0 else 1) ++ "\twent first: " ++ (if wentFirst then firstPlayer else name)
    longestName = length $ last $ sortWith length $ M.keys dat
    pad = padR (longestName+2)

totalGames dat = mSum (\(LogEntry wins _ _) -> wins) $ M.elems dat

--Outputs a summary
showSummary dat = ((show (totalGames dat) ++ " games played\n") ++) $
  ((pad "Name" ++ "\tW\tL\tW%\n") ++) $ concatFor (reverse $
    sortWith (getWinPercent . snd) $ sortWith (getTotalGames . snd) $ M.toList dat) $
  \(name, LogEntry wins losses _) ->
  pad name ++ "\t" ++ intercalate "\t" [show wins, show losses, showWinPercent wins losses] ++ "\n"
  where
    longestName = length $ last $ sortWith length $ M.keys dat
    pad = padR (longestName+2)

getWinPercent (LogEntry wins losses _) = winPercent wins losses

getTotalGames (LogEntry wins losses _) = wins - losses

showWinPercent wins losses = showRounded 2 (winPercent wins losses) ++ "%"

winPercent wins losses = 100 * fromIntegral wins / fromIntegral (wins+losses) :: Double

errMsg state file _ = stMsg state $= "ERROR: someone has opened " ++ file ++ ". Your previous game was not recorded."
warningMsg state file _ = stMsg state $= "WARNING: someone has opened " ++ file ++ ". It can't be updated while it is open."

writeLogFile_ state winner loser winnerWentFirst = flip (catchJust $ guard . isPermissionError) (errMsg state $ logFile state) $ do
  dat <- liftM (M.map sanitizeLogEntry . M.mapKeys sanitizeName) $ loadFile M.empty (logFile state)
  let dat' = insert' winner loser True winnerWentFirst $ insert' loser winner False (not winnerWentFirst) dat
  catchJust (guard . isPermissionError) (writeFile (logFile state) (show dat' ++ "\n")) (errMsg state $ logFile state)
  catchJust (guard . isPermissionError) (writeFile (logFile2 state) (showLogFile dat')) (warningMsg state $ logFile2 state)
  catchJust (guard . isPermissionError) (writeFile (summaryFile state) (showSummary dat')) (warningMsg state $ summaryFile state)
  where
    insert' :: String -> String -> Bool -> Bool -> LogFile -> LogFile
    insert' a b win wentFirst map = let winCount = if win then 1 else 0 in case (M.member a map) of
      True -> M.adjust f a map where
        f (LogEntry wins losses games) =
          LogEntry (wins+winCount) (losses+1-winCount) ((b,win,wentFirst):games)
      False -> M.insert a (LogEntry winCount (1-winCount) [(b,win,wentFirst)]) map


data HexGrid = HexGrid {
  hexHalfHeight :: R,
  hexHalfWidth :: R,
  hexQuarterWidth :: R,
  hexPos :: (R,R)
} deriving (Show)

makeHexGrid halfHeight pos = HexGrid halfHeight (quarterWidth*2) quarterWidth pos
  where quarterWidth = halfHeight / sqrt 3

hexWidth hex = hexHalfWidth hex + hexQuarterWidth hex

v_ (x,y) = v (V2 x y)

hexFill hex (x,y) clr border = do
  color clr
  renderPrimitive Polygon $ mapM_ v_ points
  color border
  renderPrimitive LineLoop $ mapM_ v_ points
  where
    halfHeight = hexHalfHeight hex
    halfWidth = hexHalfWidth hex
    quarterWidth = hexQuarterWidth hex
    width = hexWidth hex
    (bx,by) = hexPos hex
    x2 = (x - y) * width + bx
    y2 = (x + y) * halfHeight + by
    points = [(x2+quarterWidth, y2), (x2, y2+halfHeight), (x2+quarterWidth, y2+2*halfHeight),
      (x2+width, y2+2*halfHeight), (x2+2*halfWidth, y2+halfHeight), (x2+width, y2)]

--This is slightly less efficient than hexFill
hexFill' (bx,by) halfHeight clr border = do
  color clr
  renderPrimitive Polygon $ mapM_ v_ points
  color border
  renderPrimitive LineLoop $ mapM_ v_ points
  where
    quarterWidth = halfHeight / sqrt 3
    halfWidth = quarterWidth*2
    width = halfWidth + quarterWidth
    x2 = bx
    y2 = by
    points = [(x2+quarterWidth, y2), (x2, y2+halfHeight), (x2+quarterWidth, y2+2*halfHeight),
      (x2+width, y2+2*halfHeight), (x2+2*halfWidth, y2+halfHeight), (x2+width, y2)]

pixelToHex hex (x_,y_) boardSize = head $ flip sortWith
  [(x2,y2) | x2 <- [0..boardSize-1], y2 <- [0..boardSize-1]] $ \(x2_,y2_) ->
    let
      x2 = fromIntegral x2_
      y2 = fromIntegral y2_
      x3 = (x2 - y2) * width + bx + halfWidth
      y3 = (x2 + y2) * halfHeight + by + halfHeight
    in abs (x3 - x) + abs (y3 - y)
  where
    x = fromIntegral x_
    y = fromIntegral y_
    halfHeight = hexHalfHeight hex
    halfWidth = hexHalfWidth hex
    width = hexWidth hex
    (bx,by) = hexPos hex


data State = State {
  stBoard :: IOArray (Int,Int) Tile,
  stBoardSize :: Int,
  stPlayer1 :: IORef String,
  stPlayer2 :: IORef String,
  stHexGrid :: HexGrid,
  stCurPlayer :: IORef Tile,
  stMsg :: IORef String,
  stWon :: IORef Bool,
  stCursor1 :: IORef (Int,Int),
  stCursor2 :: IORef (Int,Int),
  stShowCursor1 :: IORef Bool,
  stShowCursor2 :: IORef Bool,
  stFlashCounter :: IORef (Maybe R),
  stPlayer1AI :: IORef AI,
  stPlayer2AI :: IORef AI,
  stMoveCount :: IORef Int,
  stStartPlayer :: IORef Tile,
  stGameCount :: IORef Int,
  stConfig :: Config
}

data AI = Human | Random | Adjacent | Test | Test2 deriving (Eq,Show)

nameToAI_ "random" = Random
nameToAI_ "adjacent" = Adjacent
nameToAI_ "test" = Test
nameToAI_ "test2" = Test2
nameToAI_ _ = Human
nameToAI config name = if allowAI_ config then nameToAI_ name else Human

stCursor state = do
  curPlayer <- get (stCurPlayer state)
  pure $ case curPlayer of
    Player1 -> Just $ stCursor1 state
    Player2 -> Just $ stCursor2 state
    Empty -> Nothing

stShowCursor state = do
  curPlayer <- get (stCurPlayer state)
  pure $ case curPlayer of
    Player1 -> Just $ stShowCursor1 state
    Player2 -> Just $ stShowCursor2 state
    Empty -> Nothing

stOtherCursor state = do
  curPlayer <- get (stCurPlayer state)
  pure $ case curPlayer of
    Player1 -> Just $ stShowCursor2 state
    Player2 -> Just $ stShowCursor1 state
    Empty -> Nothing

boardBounds state = ((0,0), (boardSize-1,boardSize-1))
  where boardSize = stBoardSize state

data Tile = Player1 | Player2 | Empty | Edge (Color3 GLR) | Win (Color3 GLR)
  deriving (Eq,Show)

player1Clr = rgb 0 0.75 1
player2Clr = complement player1Clr
edgeClr = grey
gridClr = grey
backClr = black

flashTime = Just 5

tileColor _ Empty = pure backClr
tileColor state (Win color) = do
  counter <- getCounter state
  pure $ if counter then lighten 0.5 color else color
tileColor _ (Edge color) = pure color
tileColor _ Player1 = pure player1Clr
tileColor _ Player2 = pure player2Clr

getCounter state = do
  counter_ <- get (stFlashCounter state)
  if isNothing counter_ then pure False else do
  let counter = fromJust counter_
  if counter <= 0 then stFlashCounter state $= Nothing >> pure False
    else pure $ if counter `posmod'` 1 >= 0.5 then True else False

locFromIntegral (x,y) = (fromIntegral x, fromIntegral y)

drawBoard state = do
  player <- get (stCurPlayer state)
  board <- getAssocs (stBoard state)
  forM_ board $ \(loc,tile) -> do
    clr <- tileColor state tile
    hexFill (stHexGrid state) (locFromIntegral loc)
      clr gridClr

  showCursor_ <- stShowCursor state
  cursor_ <- stCursor state
  if isNothing cursor_ then pure () else do
  showCursor <- get (fromJust showCursor_)
  if showCursor == False then pure () else do
  cursor <- get (fromJust cursor_)
  let tile = fromJust $ lookup cursor board
  tileClr <- tileColor state tile
  playerClr <- tileColor state player
  hexFill (stHexGrid state) (locFromIntegral cursor)
    (darken 0.25 (lerpClr 0.3 (lighten 0.25 tileClr) playerClr))
    (lighten 0.5 $ playerClr)

initState :: Config -> Int -> HexGrid -> AI -> AI -> IO State
initState config boardSize_ hex player1AI_ player2AI_ = do
  let boardSize = boardSize_ + 2
  board <- newArray ((0,0), (boardSize-1,boardSize-1)) Empty
  forM_ [1..boardSize-2] $ \i -> do
    writeArray board (0, i) (Edge player1Clr)
    writeArray board (boardSize-1, i) (Edge player1Clr)
    writeArray board (i, 0) (Edge player2Clr)
    writeArray board (i, boardSize-1) (Edge player2Clr)
  forM_ [(0,0), (0,boardSize-1), (boardSize-1,boardSize-1),
    (boardSize-1,0)] $ \pos -> writeArray board pos (Edge edgeClr)
  let hex' = hex {hexPos = ((fromIntegral $ boardSize-1) * hexWidth hex, 16)}
  curPlayer <- newIORef Player1
  player1 <- newIORef $ error "player1 not initialized"
  player2 <- newIORef $ error "player2 not initialized"
  msg <- newIORef $ error "msg not initialized"
  won <- newIORef False
  cursor1 <- newIORef (boardSize`div`2, boardSize`div`2)
  cursor2 <- newIORef (boardSize`div`2, boardSize`div`2)
  showCursor1 <- newIORef True
  showCursor2 <- newIORef True
  flashCounter <- newIORef Nothing
  player1AI <- newIORef player1AI_
  player2AI <- newIORef player2AI_
  moveCount <- newIORef 0
  startPlayer <- newIORef Player1
  gameCount <- newIORef 0
  pure $ State board boardSize player1 player2 hex' curPlayer msg won
    cursor1 cursor2 showCursor1 showCursor2 flashCounter player1AI player2AI moveCount startPlayer gameCount config

checkForWin state = do
  won <- get (stWon state)
  if won then pure () else do
  let
    boardSize = stBoardSize state
    p1src = (0,1)
    p1dest = [(boardSize-1,y) | y <- [1..boardSize-2]]
    p2src = (1,0)
    p2dest = [(x,boardSize-1) | x <- [1..boardSize-2]]
  (p1win,_,p1WinPath) <- search state [p1src] p1dest [Player1, Edge player1Clr] [] []
  (p2win,_,p2WinPath) <- search state [p2src] p2dest [Player2, Edge player2Clr] [] []
  p1 <- get (stPlayer1 state)
  p2 <- get (stPlayer2 state)
  when p1win $ win state p1 p2 player1Clr Player1 p1WinPath
  when p2win $ win state p2 p1 player2Clr Player2 p2WinPath

nextTo (x,y) (x',y') = cond'' False (x',y') [
  ((x+1,y), True),
  ((x-1,y), True),
  ((x,y+1), True),
  ((x,y-1), True),
  ((x-1,y-1), True),
  ((x+1,y+1), True)
  ]

mkLines :: [a] -> [(a,a)]
mkLines [] = []
mkLines (x:y:xs) = (x,y) : mkLines (y:xs)
mkLines _ = []

simplifyPath [x] = [x]
simplifyPath [x,y] = [x,y]
simplifyPath (x:y:z:xs)
  | nextTo x z = simplifyPath (x:z:xs)
  | True = let path' = x : simplifyPath (y:z:xs) in if (x:y:z:xs) == path'
    then path' else simplifyPath path'

simplifyPath2 path = simplifyPath $ fixPath path

fixPath [] = []
fixPath [x] = [x]
fixPath (x:y:xs)
  | nextTo x y = let path' = x : fixPath (y:xs) in if (x:y:xs) == path'
    then path' else fixPath path'
  | True = fixPath (x : xs)

win state winner loser winnerTile winner' path = do
  let path' = simplifyPath2 $ filter (onBoard' state) path
  --forM_ (filter (onBoard' state) path) $ \loc -> writeArray (stBoard state) loc (Edge grey)
  --forM_ (addIndices path') $ \(i,loc) -> writeArray (stBoard state) loc
  --  (Win $ fmap (*(fromIntegral i/fromIntegral (length path'))) winnerTile)
  forM_ path' $ \loc -> writeArray (stBoard state) loc (Win winnerTile)
  stGameCount state $~ succ
  gameCount <- get (stGameCount state)
  if Just gameCount == maxGames state
    then stMsg state $= winner ++ " wins! Press 'ESC' to quit and choose another opponent."
    else stMsg state $= winner ++ " wins! Press 'r' to play again, or 'ESC' to quit."
  stShowCursor1 state $= False
  stShowCursor2 state $= False

  startPlayer <- get (stStartPlayer state)
  let winnerWentFirst = winner' == startPlayer

  when (writeLogFiles state) $ writeLogFile state winner loser winnerWentFirst
  stCurPlayer state $= Empty --prevents anything from being highlighted after someone wins
  stWon state $= True
  stFlashCounter state $= flashTime

adjacent state (x,y) = filter (onBoard state) $
  [(x-1,y), (x+1,y), (x,y-1), (x,y+1), (x-1,y-1), (x+1,y+1)]

search state locs dests opts alreadySearched winningPath =
  if null locs then pure (False,alreadySearched,winningPath) else do
  if head locs `elem` dests then pure (True,alreadySearched,winningPath) else do
  let
    loc = head locs
    locs' = filter (`notElem` alreadySearched) $ adjacent state loc
  locs'' <- filterM (tileAt state opts) locs'

  (won0,alreadySearched',winningPath') <- search state locs'' dests opts (loc:alreadySearched) winningPath
  (won1,alreadySearched'',winningPath'') <- search state (tail locs) dests opts (alreadySearched') winningPath'

  tile <- getTile state (head locs)
  player <- get (stCurPlayer state)
  let winningPath''' = if won0 then [head locs] else []
  pure (won0 || won1, alreadySearched'', winningPath'' ++ winningPath''')

getTile state loc = readArray (stBoard state) loc

tileAt state opts loc = do
  tile <- getTile state loc
  pure $ tile `elem` opts

onBoard state (x,y) = x >= 0 && y >=0 && x < size && y < size
  where size = stBoardSize state

onBoard' state (x,y) = x >= 1 && y >= 1 && x < size-1 && y < size-1
  where size = stBoardSize state

onBoard'_ (x,y) = x >= 1 && y >= 1 && x < size-1 && y < size-1
  where size = boardSize+2


loop state = do
  quitKey <- getKey $ SpecialKey ESC
  when (quitKey == Press) $ terminate >> exitSuccess

  orthoPixels

  renderText white 0 =<< get (stMsg state)
  drawBoard state

  V2 w h <- screenSize
  mousePos $~ \(Position x y) -> Position (limit 0 (round w) x) (limit 0 (round h) y)

  won <- get (stWon state)
  unless won $ do
    (Position x y) <- get mousePos
    clr <- tileColor state =<< get (stCurPlayer state)
    let
      halfHeight = hexSize/2
      quarterWidth = halfHeight / sqrt 3
      halfWidth = quarterWidth*2
    hexFill' (fromIntegral x - halfWidth, fromIntegral y - halfHeight) halfHeight clr gridClr

  let
    textXPos1 = 300
    textYPos1 = 125
    textXPos2 = 350
    textYPos2 = 125

  p1 <- get (stPlayer1 state)
  p2 <- get (stPlayer2 state)
  renderText' player1Clr (V2 (textXPos1 - 8 * fromIntegral (length p1)) (textYPos1+16)) p1
  renderText' player2Clr (V2 (w-textXPos1) (textYPos1+16)) p2

  let p1Controls = "Controls: mouse or number pad"
  let p2Controls = "Controls: mouse or QWEASD and spacebar"
  renderText' player1Clr (V2 (w-textXPos2) (h-textYPos2)) p1Controls
  renderText' player2Clr (V2 (textXPos2 - 8 * fromIntegral (length p2Controls)) (h-textYPos2)) p2Controls

  stFlashCounter state $~ liftM (subtract (1/fps))

  updateDisplay
  advanceTime (const $ pure ()) 0 fps
  loop state

fps = 60

boardSize = 11
hexSize = 28 --This should be an even number or the cursor will be slightly blurry

getPlayerName_ clr player entryDone curEntry = do
  done <- get entryDone
  entry <- get curEntry
  if done then pure (sanitizeName entry) else do

  quitKey <- getKey $ SpecialKey ESC
  when (quitKey == Press) $ terminate >> exitSuccess

  orthoPixels

  renderText clr 0 $ "Enter " ++ player ++ "'s name: " ++ entry

  updateDisplay
  advanceTime (const $ pure ()) 0 30
  getPlayerName_ clr player entryDone curEntry

getPlayerName clr player = do
  entryDone <- newIORef False
  curEntry <- newIORef ""
  keyCallback $= entryKeyPress entryDone curEntry
  charCallback $= entryCharPress entryDone curEntry
  getPlayerName_ clr player entryDone curEntry

entryKeyPress _ curEntry (SpecialKey BACKSPACE) Press =
  curEntry $~ \entry -> if null entry then "" else init entry
entryKeyPress entryDone _ (SpecialKey ENTER) Press =
  entryDone $= True
entryKeyPress _ _ _ _ = pure ()

entryCharPress _ curEntry c Press =
  curEntry $~ (++[c])
entryCharPress _ _ _ _ = pure ()

--If the first player is an AI, let them make the first move
firstAIMove state = do
  curPlayer <- get (stCurPlayer state)
  otherPlayer <- stOtherPlayer state
  ai <- get $ if curPlayer == Player1 then stPlayer1AI state else stPlayer2AI state
  unless (ai==Human) $ doAI state ai otherPlayer curPlayer


main = do
  config_ <- readConfig configFile
  config <- case config_ of
    Nothing -> do
      putStrLn "Warning: config file not found; using default settings."
      pure $ List [
        Var "writeLogFiles" (Boolean False),
        Var "hideCursor" (Boolean True),
        Var "limitGames" (Boolean False),
        Var "allowAI" (Boolean True)
        ]
    Just config -> pure config
  let hex = makeHexGrid hexSize (0,0)
  let
    boardSize' = boardSize + 2
    w = fromIntegral boardSize' * 2 * hexWidth hex - hexHalfWidth hex
    h = fromIntegral boardSize' * 2 * hexHalfHeight hex + 16
  initDisplay (V2 w h) "Hex" False
  when (hideCursor_ config) $ disableSpecial MouseCursor

  player1 <- getPlayerName player1Clr "player 1"
  player2 <- getPlayerName player2Clr "player 2"
  state <- initState config boardSize hex (nameToAI config $ map toLower player1) (nameToAI config $ map toLower player2)
  stMsg state $= "It is " ++ player1 ++ "'s turn."
  stPlayer1 state $= player1
  stPlayer2 state $= player2

  mouseButtonCallback $= mousePress state
  keyCallback $= keyPress state
  --we have to reset this callback because getPlayerName sets it
  charCallback $= \_ _ -> pure ()

  firstAIMove state

  loop state

playerName state player = get $ if player == Player1 then stPlayer1 state else stPlayer2 state

move state loc = do
  empty <- isEmpty state loc
  curPlayer <- get (stCurPlayer state)
  didAI <- if not empty then pure False else do
    writeArray (stBoard state) loc curPlayer
    stMoveCount state $~ succ

    otherPlayer <- stOtherPlayer state
    name <- playerName state otherPlayer

    moveCount <- get (stMoveCount state)

    --We set this message early because otherwise it can overwrite the win message
    stMsg state $= "It is " ++ name ++ "'s turn."
    checkForWin state
    otherPlayer <- stOtherPlayer state
    stCurPlayer state $= otherPlayer
    won <- get (stWon state)
    if won then pure False else do
      ai <- get $ if otherPlayer == Player1 then stPlayer1AI state else stPlayer2AI state
      unless (ai==Human) $ doAI state ai otherPlayer curPlayer
      pure (ai/=Human)
  pure (empty,didAI)

stOtherPlayer state = do
  curPlayer <- get (stCurPlayer state)
  pure $ otherPlayer curPlayer

otherPlayer player = if player == Player1 then Player2 else Player1

doAI state ai me opponent = do
  loc <- case ai of
    Human -> error "doAI on human!"
    Random -> randomAI state me opponent
    Adjacent -> adjacentAI 0 state me opponent
    Test -> testAI 0 state me opponent
    Test2 -> test2AI state me opponent
  (moved,_) <- move state loc
  unless moved $ error $ show ai ++ " tried to play on an empty tile!"

randomAI state me opponent = do
  loc <- randLoc state
  tile <- getTile state loc
  if tile == Empty then pure loc else randomAI state me opponent

adjacentAI count state me opponent = do
  if count == 1000 then randomAI state me opponent else do
  loc <- randLoc state
  tile <- getTile state loc
  tiles <- mapM (getTile state) $ adjacent state loc
  if any (==opponent) tiles && tile == Empty then pure loc else adjacentAI (count+1) state me opponent

testAI count state me opponent = do
  if count == 1000 then randomAI state me opponent else do
  loc@(x,y) <- randLoc state
  tile <- getTile state loc
  tiles <- mapM (getTile state) $ filter (\(x',y') -> x'>x || y'>y) $ adjacent state loc
  if any (==opponent) tiles && tile == Empty then pure loc else testAI (count+1) state me opponent

{-
  This AI will work by measuring how many moves it will take to win after making a particular move.
  It will use a search algorithm to measure the distance for each possible move. It will run the
  search twice, from a candidate square to each edge. Empty tiles have cost 1 and red tiles
  have cost 0.
-}
test2AI state me opponent = do
  wholeboard <- wholeBoard state
  empty <- emptyTiles state
  let adjacent = filter (f wholeboard) empty
  let adjacent' = filter (f' wholeboard) empty
  if not (null adjacent) then randomElemIO adjacent else
    if not (null adjacent') then randomElemIO adjacent' else randomAI state me opponent
  where
    f board (x,y) = any (\loc -> (fromJust $ lookup loc board)==me) $ filter onBoard'_ $
      if me == Player2
        then [(x,y-1), (x,y+1), (x-1,y-1), (x+1,y+1)]
        else [(x-1,y), (x+1,y), (x-1,y-1), (x+1,y+1)]
    f' board (x,y) = any (\loc -> (fromJust $ lookup loc board)==me) $ filter onBoard'_ $
      [(x-1,y), (x+1,y), (x,y-1), (x,y+1), (x-1,y-1), (x+1,y+1)]

emptyTiles state = do
  wholeboard <- wholeBoard state
  pure $ map fst $ (filter $ (==Empty) . snd) wholeboard

wholeBoard state =  mapM (\loc -> liftM (loc,) (getTile state loc)) $
  range ((0,0), (boardSize+1, boardSize+1))


randLoc_ (x0,y0) (x1,y1) = do
  x <- randomRIO (x0,x1)
  y <- randomRIO (y0,y1)
  pure (x,y)

randLoc state = randLoc_ (1,1) (vsub (pairOf $ stBoardSize state) (1,1))

mousePress state ButtonLeft Press = do
  won <- get (stWon state)
  if won then pure () else do
  (Position x y) <- get mousePos
  let pos = pixelToHex (stHexGrid state) (x,y) (stBoardSize state)
  (moved,didAI) <- move state pos
  when moved $ do
    showCursor <- if didAI then stShowCursor state else stOtherCursor state
    when (isJust showCursor) $ fromJust showCursor $= False
mousePress _ _ _ = pure ()

reset state = do
  player1 <- get (stPlayer1 state)
  player2 <- get (stPlayer2 state)
  let hex = makeHexGrid hexSize (0,0)
  newState <- initState (stConfig state) boardSize hex
    (nameToAI (stConfig state) $ map toLower player1) (nameToAI (stConfig state) $ map toLower player2)
  copyArray (stBoard newState) (stBoard state)
  copyIORef (stCursor1 newState) (stCursor1 state)
  copyIORef (stCursor2 newState) (stCursor2 state)
  stStartPlayer state $~ otherPlayer
  startPlayer <- get (stStartPlayer state)
  stCurPlayer state $= startPlayer
  name <- playerName state startPlayer
  stMsg state $= "It is " ++ name ++ "'s turn."
  copyIORef (stWon newState) (stWon state)
  copyIORef (stFlashCounter newState) (stFlashCounter state)
  stShowCursor1 state $= True
  stShowCursor2 state $= True
  firstAIMove state

keyPress state (CharKey 'R') Press = do
  gameCount <- get (stGameCount state)
  won <- get (stWon state)
  if Just gameCount == maxGames state
    then stMsg state $= "You can't play again. You have already played " ++ show (fromJust $ maxGames state) ++ " games. Choose another opponent."
    else if won
      then reset state
      else stMsg state $= "You can't reset until the current game is over."
keyPress state key Press = do
  won <- get (stWon state)
  if won then pure () else do
  case key of
    CharKey 'Q' -> moveCursor Player2 state (-1,0)
    CharKey 'W' -> moveCursor Player2 state (-1,-1)
    CharKey 'E' -> moveCursor Player2 state (0,-1)
    CharKey 'A' -> moveCursor Player2 state (0,1)
    CharKey 'S' -> moveCursor Player2 state (1,1)
    CharKey 'D' -> moveCursor Player2 state (1,0)
    CharKey ' ' -> doCursor Player2 state
    SpecialKey KP_7 -> moveCursor Player1 state (-1,0)
    SpecialKey KP_8 -> moveCursor Player1 state (-1,-1)
    SpecialKey KP_9 -> moveCursor Player1 state (0,-1)
    SpecialKey KP_4 -> moveCursor Player1 state (0,1)
    SpecialKey KP_5 -> moveCursor Player1 state (1,1)
    SpecialKey KP_6 -> moveCursor Player1 state (1,0)
    SpecialKey KP_1 -> moveCursor Player1 state (0,1)
    SpecialKey KP_2 -> moveCursor Player1 state (1,1)
    SpecialKey KP_3 -> moveCursor Player1 state (1,0)
    SpecialKey KP_ENTER -> doCursor Player1 state
    SpecialKey KP_0 -> doCursor Player1 state
    _ -> pure ()
keyPress _ _ _ = pure ()

doCursor player state = do
  curPlayer <- get (stCurPlayer state)
  if curPlayer /= player then pure () else do
  showCursor <- stShowCursor state
  when (isJust showCursor) $ fromJust showCursor $= True
  cursor_ <- stCursor state
  if isNothing cursor_ then pure () else let cursor' = fromJust cursor_ in
    void $ move state =<< get cursor'

moveCursor player state (dx,dy) = do
  curPlayer <- get (stCurPlayer state)
  if curPlayer /= player then pure () else do
  showCursor <- stShowCursor state
  when (isJust showCursor) $ fromJust showCursor $= True
  cursor_ <- stCursor state
  if isNothing cursor_ then pure () else let cursor = fromJust cursor_ in do
  cursor $~ \(x,y) -> let new = (x+dx,y+dy) in
    if onBoard' state new then new else (x,y)

--doesn't check to make sure the arrays are the same size
copyArray old new = do
  assocs <- getAssocs old
  forM_ assocs $ \(i,val) -> writeArray new i val

copyIORef old new = do
  val <- get old
  new $= val

isEmpty state loc = do
  tile <- getTile state loc
  pure $ tile == Empty
