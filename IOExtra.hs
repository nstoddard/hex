module IOExtra where

import Control.Monad
import Control.Applicative
import System.Directory
import System.Time
import System.Cmd
import System.IO.Error
import Control.Exception hiding (catch)
import System.IO
import System.Exit
import Data.IORef
import Data.Char

import MonadExtra

newLine :: IO ()
newLine = putStrLn ""

flush :: IO ()
flush = hFlush stdout

prompt :: String -> IO String
prompt msg = do
  if last msg == '\n'
    then putStr msg
    else putStr msg >> flush
  getLine

prompt' :: [(Char,b)] -> String -> IO b
prompt' xs msg = do
  response <- prompt msg
  if null response
    then prompt' xs msg
    else maybe (prompt' xs msg) pure $
      lookup (toLower $ head response) xs

yesno = prompt' [('y',True),('n',False)]

-- |adds trailing '/' if not already present
-- make sure the path is a directory before calling this (it will happily add
-- a slash to the end of a filename)
addSlash :: FilePath -> FilePath
addSlash x
  | last x == '/' = x
  | True = x++"/"

getSubdirs :: FilePath -> IO [FilePath]
getSubdirs dir = filterM doesDirectoryExist =<<
  pure . map (addSlash dir++) =<<
  pure . filter ((/='.') . head) =<<
  pure . filter (`notElem` [".",".."]) =<<
  getDirectoryContents dir

getAllSubdirs :: FilePath -> IO [FilePath]
getAllSubdirs dir = liftM (dir:) $ getAllSubdirs' dir where
  getAllSubdirs' dir = do
    subdirs <- getSubdirs dir
    subdirs' <- if null subdirs
      then pure []
      else concatMapM getAllSubdirs' subdirs
    pure (subdirs++subdirs')

getSubfiles :: FilePath -> IO [FilePath]
getSubfiles dir = filterM doesFileExist =<<
  pure . map (addSlash dir++) =<<
  pure . filter ((/='.') . head) =<<
  pure . filter (`notElem` [".",".."]) =<<
  getDirectoryContents dir

getAllSubfiles :: FilePath -> IO [FilePath]
getAllSubfiles = concatMapM getSubfiles <=< getAllSubdirs

--TODO: find better name
loadFile :: Read a => a -> FilePath -> IO a
loadFile defaultContent path = do
  exists <- doesFileExist path
  if exists
    then withFile path ReadMode $ \file -> liftM read (hGetLine file)
    else pure defaultContent

modifyIORef' :: IORef a -> (a->IO a) -> IO ()
modifyIORef' ref f = do
  val <- readIORef ref
  val' <- f val
  writeIORef ref val'

cloneIORef :: IORef a -> IO (IORef a)
cloneIORef = newIORef <=< readIORef

printAndExit :: String -> IO a
printAndExit errMsg = putStrLn errMsg >> exitFailure

catchEOF :: IO a -> IO a -> IO a
catchEOF act subst = catchJust (guard . isEOFError)
  act (const subst)

getCalendarTime :: IO CalendarTime
getCalendarTime = toCalendarTime =<< getClockTime

-- |Gets the clock time (in seconds) as a Double
getClockTime' :: IO Double
getClockTime' = do
  TOD sec picosec <- getClockTime
  pure $ fromIntegral sec + 1.0e-12 * fromIntegral picosec
