{-# LANGUAGE NoMonomorphismRestriction #-}

module OldGUI where

import Unsafe.Coerce
import Control.Applicative
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad
import System.Exit
import Data.IORef
import System.IO.Unsafe

import NumExtra
import Vect

type GLR = GLdouble

--The global variables
--(They should be local to this file, so no harm done)
screenSize_ :: IORef (V2 R)
screenSize_ = unsafePerformIO (newIORef $ error "screenSize_ not initialized")
screenSizeD_ :: IORef (V2 Double)
screenSizeD_ = unsafePerformIO (newIORef $ error "screenSizeD_ not initialized")
screenCenter_ :: IORef (V2 R)
screenCenter_ = unsafePerformIO (newIORef $ error "screenCenter_ not initialized")

screenSize :: IO (V2 R)
screenSize = get screenSize_

screenCenter :: IO (V2 R)
screenCenter = get screenCenter_

--inefficient
toPosition :: V2 R -> Position
toPosition (V2 x y) = Position (round x) (round y)

fromPosition :: Position -> V2 R
fromPosition (Position x y) = V2 (fromIntegral x) (fromIntegral y)

use3D_ :: IORef Bool
use3D_ = unsafePerformIO (newIORef undefined)


-- realToFrac is too inefficient to use here
floatCast :: R -> GLR
floatCast = unsafeCoerce
{-# INLINE floatCast #-}

floatCast_ :: GLR -> R
floatCast_ = unsafeCoerce
{-# INLINE floatCast_ #-}

--for the few OpenGL functions that need doubles rather than floats
floatCastD :: Double -> GLdouble
floatCastD = unsafeCoerce
{-# INLINE floatCastD #-}

floatCastD_ :: GLdouble -> Double
floatCastD_ = unsafeCoerce
{-# INLINE floatCastD_ #-}

intCast :: Int -> GLint
intCast = unsafeCoerce
{-# INLINE intCast #-}

intCast_ :: GLint -> Int
intCast_ = unsafeCoerce
{-# INLINE intCast_ #-}


argb :: GLR -> GLR -> GLR -> GLR -> Color4 GLR
argb = Color4
rgb :: GLR -> GLR -> GLR -> Color3 GLR
rgb = Color3
gscale x = rgb x x x

red = rgb 1 0 0
green = rgb 0 1 0
blue = rgb 0 0 1
yellow = rgb 1 1 0
cyan = rgb 0 1 1
magenta = rgb 1 0 1
white = gscale 1
black = gscale 0
grey = gscale 0.5
lightgrey = gscale 0.75
darkgrey = gscale 0.25
orange = rgb 1 0.5 0
transparent = argb 0 0 0 0

complement (Color3 r g b) = Color3 (1-r) (1-g) (1-b)

--TODO: move to libs
lighten amount (Color3 r g b) = Color3 (f r) (f g) (f b) where
  f x = 1 - (1-x) * (1-amount)

darken amount (Color3 r g b) = Color3 (f r) (f g) (f b) where
  f x = x * (1-amount)

vToSize (V2 x y) = Size (floor x) (floor y)

lerpClr amount (Color3 r g b) (Color3 r' g' b') = Color3 (f r r') (f g g') (f b b') where
  f x y = x * (1-amount) + y * amount

--These three functions require GLdoubles instead of GLfloats
orthoV (V2 x y) (V2 x' y') =
  ortho2D (floatCastD x) (floatCastD x')
    (floatCastD y) (floatCastD y')
perspective' angle near far = do
  (V2 xs ys) <- get screenSizeD_
  perspective (floatCastD $ radToDeg angle) (floatCastD $ xs/ys)
    (floatCastD near) (floatCastD far)

orthoV' a b = orthoV (fmap realToFrac a) (fmap realToFrac b)

--unproject :: Position -> IO (Double,Double)
unproject (Position x y) = do
  model <- get $ matrix $ Just (Modelview 0)
  project <- get $ matrix $ Just Projection
  view <- get viewport
  (Vertex3 x' y' _) <- unProject
    (Vertex3 (realToFrac x) (realToFrac y) 0)
    (model::GLmatrix GLdouble) project view
  --y must be flipped for some reason
  pure (floatCastD_ x', -floatCastD_ y')


rotate' angle = GL.rotate (floatCast $ radToDeg angle) . vector3
translate' = translate . vector3

v :: V2 R -> IO ()
v (V2 x y) = vertex $ Vertex2 (floatCast x) (floatCast y)

vInt :: V2 Int -> IO ()
vInt (V2 x y) = vertex $ Vertex2 (intCast x) (intCast y)

vector2 (V2 x y) = Vector3 (floatCast x) (floatCast y) 0
{-# INLINE vector2 #-}

vector3 (V3 x y z) = Vector3 (floatCast x) (floatCast y) (floatCast z)
{-# INLINE vector3 #-}


--The only font GLFW offers is 8×16
textW = 8
textH = 16

--Offers less control over formatting than renderText', but is easier to
--use.
renderText clr (V2 x y) strs = do
  let strs' = lines strs
  forM_ (zip strs' [0..]) $ \(str,y')->
    renderText' clr (V2 (x*textW) ((y + y')*textH)) str

--Offers a bit more control over formatting than renderText
renderText' clr (V2 x y) str = preservingMatrix $ do
  color clr
  loadIdentity
  (V2 _ yMax) <- get screenSize_
  V2 scrW scrH <- get screenSizeD_
  orthoV (V2 0 0) (V2 (scrW-1) (scrH-1)) --for some reason we must subtract 1 from the screen size
  translate $ vector2 (V2 x (yMax-y-textH))
  withoutTextures $ renderString Fixed8x16 str

withoutTextures f = do
  textured <- get (texture Texture2D)
  texture Texture2D $= Disabled
  f
  when (textured==Enabled) $ texture Texture2D $= Enabled

orthoPixels = do
  (V2 x y) <- get screenSizeD_
  orthoV (V2 0.5 (y+0.5)) (V2 (x+0.5) 0.5)

{-makeCircle r nSegs angle pos =
  map (\i -> let r2 = i*coef in
    fromPolar (V2 r (r2+angle)) + pos) [0..nSegs]
  where coef = tau/realToFrac nSegs

drawCircle typ r nSegs pos = renderPrimitive typ $ mapM v $
  makeCircle r nSegs 0 pos-}

makeCircle r nSegs angle pos =
  map (\i -> let r2 = i*coef in
    fromPolar (V2 r (r2+angle)) + pos) [0..nSegs]
  where coef = tau/realToFrac nSegs

cachedCircle :: IORef [V2 R]
cachedCircle = unsafePerformIO $ newIORef
  (error "You need to call cacheCircle before drawCircle")

cacheCircle nSegs = cachedCircle $= makeCircle 1 nSegs 0 0

drawCircle :: PrimitiveMode -> R -> V2 R -> IO ()
drawCircle typ r (V2 x y) = preservingMatrix $ do
  translate (Vector3 (floatCast x) (floatCast y) 0)
  GL.scale (floatCast r) (floatCast r) 1
  circle <- get cachedCircle
  renderPrimitive typ $ mapM_ v circle

drawCircle' inside border r loc = do
  color inside
  drawCircle Polygon r loc
  color border
  drawCircle LineLoop r loc

initDisplay screenSize@(V2 x y) title use3D = do
  screenSize_ $= screenSize
  screenSizeD_ $= V2 (realToFrac x) (realToFrac y)
  screenCenter_ $= fmap (/2) screenSize
  use3D_ $= use3D

  success <- initialize
  unless success $ putStrLn "Unable to initialize GLFW." >> exitFailure
  success2 <- openWindow (vToSize screenSize) [DisplayDepthBits 16] Window
  unless success2 $ putStrLn "Unable to open window." >> exitFailure
  windowTitle $= title

  pointSmooth $= Enabled
  pointSize $= 3
  lineSmooth $= Enabled
  lineWidth $= 1
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  matrixMode $= Projection
  clearColor $= Color4 0 0 0 1
  if use3D
    then do
      clear [ColorBuffer, DepthBuffer]
      depthFunc $= Just Lequal
    else clear [ColorBuffer]
  loadIdentity

  windowCloseCallback $= exitSuccess
  windowSizeCallback $= (\size -> viewport $= (Position 0 0, size))
  checkErrors "initDisplay"

--TODO: choose one of the following implementations of advanceTime
--They are basically identical, but with different variable names
{-advanceTime stateVar t = do
  t' <- get time
  let framesPassed = truncate $ desiredFPS*(t'-t)
  let simulNewTime = t + toEnum framesPassed / desiredFPS
  --physics stateVar

  t'' <- get time
  let diff = t'' - simulNewTime
  let sleepTime = 1/desiredFPS - diff
  when (sleepTime > 0) $ sleep sleepTime
  pure simulNewTime-}


advanceTime doPhysics oldTime' desiredFPS = do
  let oldTime = realToFrac oldTime'
  let framePeriod = 1 / realToFrac desiredFPS
  newTime <- get time
  let mult = {-frameSteps /-} 1 / framePeriod
  let framesPassed = truncate $ mult * (newTime - oldTime)
  let simulNewTime = oldTime + toEnum framesPassed / mult

  newTime' <- get time

  doPhysics $ realToFrac newTime'

  let diff = newTime' - simulNewTime
  let sleepTime = framePeriod - diff
  when (sleepTime > 0) $ sleep sleepTime
  pure $ realToFrac simulNewTime

checkErrors str = do
  errs <- get errors
  unless (null errs) $ do
    putStrLn $ "Errors from OpenGL in " ++ str ++ ":"
    mapM_ print errs

updateDisplay = do
  checkErrors "updateDisplay"
  pollEvents

  swapBuffers
  useDepth <- get use3D_
  if useDepth
    then clear [ColorBuffer, DepthBuffer]
    else clear [ColorBuffer]
  loadIdentity

--Line renderPrimitive, but rounds to pixel borders
--Rather inefficient, and can't be used unless OpenGL-units are the same
--as pixels.
vRound (V2 x y) = v
  (V2 (fromIntegral (floor x)) (fromIntegral (floor y)))

--Like vRound, but only rounds on the y axis
vRoundY (V2 x y) = v
  (V2 x (fromIntegral (floor y)))

convertPos :: GL.Position -> V2 R
convertPos (GL.Position x y) = V2 (realToFrac x) (realToFrac y)

mPos = liftM convertPos (get mousePos)


face a b c d = mapM_ vertex [a,b,c,d]
{-# INLINE face #-}

texFace (V2 sx sy) (V2 ex ey) a b c d = do
  texCoord $ TexCoord2 sx sy
  vertex d
  texCoord $ TexCoord2 ex sy
  vertex a
  texCoord $ TexCoord2 ex ey
  vertex b
  texCoord $ TexCoord2 sx ey
  vertex c
{-# INLINE texFace #-}

--Draws a 2D texture
texFace2 texStart texEnd w' = do
  let w = floatCast w'
  texFace (fmap floatCast texStart) (fmap floatCast texEnd)
    (Vertex3 w (-w) 0) (Vertex3 w w 0)
    (Vertex3 (-w) w 0) (Vertex3 (-w) (-w) 0)
  

solidCube w' = renderPrimitive Quads $ do
  let w = floatCast w'
  face (Vertex3 w w w) (Vertex3 w w (-w))
    (Vertex3 w (-w) (-w)) (Vertex3 w (-w) w)
  face (Vertex3 w w w) (Vertex3 w w (-w))
    (Vertex3 (-w) w (-w)) (Vertex3 (-w) w w)
  face (Vertex3 w w w) (Vertex3 w (-w) w)
    (Vertex3 (-w) (-w) w) (Vertex3 (-w) w w)
  face (Vertex3 (-w) w w) (Vertex3 (-w) w (-w))
    (Vertex3 (-w) (-w) (-w)) (Vertex3 (-w) (-w) w)
  face (Vertex3 w (-w) w) (Vertex3 w (-w) (-w))
    (Vertex3 (-w) (-w) (-w)) (Vertex3 (-w) (-w) w)
  face (Vertex3 w w (-w)) (Vertex3 w (-w) (-w))
    (Vertex3 (-w) (-w) (-w)) (Vertex3 (-w) w (-w))

texCube texRes@(V2 xr yr) (V2 xp yp) (V2 xs ys) w' =
  renderPrimitive Quads $ do
  let w = floatCast w'
  let
    texPos = V2 (xp/xr) (-yp/yr) --it seems you can subtract the y-val from almost anything and it will still work
    texSize = V2 (xs/xr) (-ys/yr)
    texStart = fmap floatCast texPos
    texEnd = texStart + fmap floatCast texSize
  texFace texStart texEnd (Vertex3 w w w) (Vertex3 w w (-w))
    (Vertex3 w (-w) (-w)) (Vertex3 w (-w) w)
  texFace texStart texEnd (Vertex3 w w w) (Vertex3 w w (-w))
    (Vertex3 (-w) w (-w)) (Vertex3 (-w) w w)
  texFace texStart texEnd (Vertex3 w w w) (Vertex3 w (-w) w)
    (Vertex3 (-w) (-w) w) (Vertex3 (-w) w w)
  texFace texStart texEnd (Vertex3 (-w) w w) (Vertex3 (-w) w (-w))
    (Vertex3 (-w) (-w) (-w)) (Vertex3 (-w) (-w) w)
  texFace texStart texEnd (Vertex3 w (-w) w) (Vertex3 w (-w) (-w))
    (Vertex3 (-w) (-w) (-w)) (Vertex3 (-w) (-w) w)
  texFace texStart texEnd (Vertex3 w w (-w)) (Vertex3 w (-w) (-w))
    (Vertex3 (-w) (-w) (-w)) (Vertex3 (-w) w (-w))

type TexLoc = (V2 GLR,V2 GLR)
type TexLocs = [TexLoc]

--I don't know if this actually improves performance at all :(
--TODO: refactor
calcTexCube' :: V2 R -> V2 R -> V2 R -> TexLoc
calcTexCube' texRes@(V2 xr yr) (V2 xs ys) texPos = texFace' texPos
  where
    texSize = V2 (xs/xr) (-ys/yr)
    texFace' (V2 xp yp) = let texStart = fmap floatCast (V2 (xp/xr) (-yp/yr)) in
      (texStart, texStart + fmap floatCast texSize)

texCube' :: TexLocs -> R -> IO ()
texCube' locs w' = renderPrimitive Quads $ do
  let w = floatCast w'
  let texFace' = uncurry texFace
  texFace' (head locs) (Vertex3 w w w) (Vertex3 w w (-w))
    (Vertex3 w (-w) (-w)) (Vertex3 w (-w) w)
  texFace' (locs!!1) (Vertex3 (-w) w w) (Vertex3 (-w) w (-w))
    (Vertex3 w w (-w)) (Vertex3 w w w)
  texFace' (locs!!2) (Vertex3 w w w) (Vertex3 w (-w) w)
    (Vertex3 (-w) (-w) w) (Vertex3 (-w) w w)
  texFace' (locs!!3) (Vertex3 (-w) (-w) w) (Vertex3 (-w) (-w) (-w))
    (Vertex3 (-w) w (-w)) (Vertex3 (-w) w w)
  texFace' (locs!!4) (Vertex3 w (-w) w) (Vertex3 w (-w) (-w))
    (Vertex3 (-w) (-w) (-w)) (Vertex3 (-w) (-w) w)
  texFace' (locs!!5) (Vertex3 w w (-w)) (Vertex3 w (-w) (-w))
    (Vertex3 (-w) (-w) (-w)) (Vertex3 (-w) w (-w))


{-texCube' texRes@(V2 xr yr) texPoses (V2 xs ys) w' =
  renderPrimitive Quads $ do
  let w = floatCast w'
  let
    texSize = V2 (xs/xr) (-ys/yr)
    texFace' (V2 xp yp) =
      let texStart = fmap floatCast (V2 (xp/xr) (-yp/yr)) in
      texFace texStart (texStart + fmap floatCast texSize)
  texFace' (head texPoses) (Vertex3 w w w) (Vertex3 w w (-w))
    (Vertex3 w (-w) (-w)) (Vertex3 w (-w) w)
  texFace' (texPoses!!1) (Vertex3 (-w) w w) (Vertex3 (-w) w (-w))
    (Vertex3 w w (-w)) (Vertex3 w w w)
  texFace' (texPoses!!2) (Vertex3 w w w) (Vertex3 w (-w) w)
    (Vertex3 (-w) (-w) w) (Vertex3 (-w) w w)
  texFace' (texPoses!!3) (Vertex3 (-w) (-w) w) (Vertex3 (-w) (-w) (-w))
    (Vertex3 (-w) w (-w)) (Vertex3 (-w) w w)
  texFace' (texPoses!!4) (Vertex3 w (-w) w) (Vertex3 w (-w) (-w))
    (Vertex3 (-w) (-w) (-w)) (Vertex3 (-w) (-w) w)
  texFace' (texPoses!!5) (Vertex3 w w (-w)) (Vertex3 w (-w) (-w))
    (Vertex3 (-w) (-w) (-w)) (Vertex3 (-w) w (-w))-}


vertex3' (x,y,z) = Vertex3 x y z

wireCube w' = do
  let w = floatCast w'
  let f as = vertex (vertex3' as)
  renderPrimitive LineStrip $ do
    f (w,w,w)
    f (w,w,-w)
    f (w,-w,-w)
    f (w,-w,w)
    f (w,w,w)
    f (-w,w,w)
    f (-w,w,-w)
    f (-w,-w,-w)
    f (-w,-w,w)
    f (-w,w,w)
  renderPrimitive Lines $ do
    f (w,w,-w)
    f (-w,w,-w)
    f (w,-w,-w)
    f (-w,-w,-w)
    f (w,-w,w)
    f (-w,-w,w)


--Textures

type Texture = TextureObject

--The file MUST be a .tga, and it MUST have a power-of-two width and
--height! If it doesn't, it will probably not display correctly.
loadTexture file = do
  [tex] <- genObjectNames 1
  textureBinding Texture2D $= Just tex
  loadTexture2D file []
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  --These two lines give OpenGL errors
  --textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  --textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  checkErrors "loadTexture"
  return tex

setTexture tex = textureBinding Texture2D $= Just tex



--A utility function for non-interactive animations
animate :: (Real a, Fractional a, Real b) => (a->IO c) -> b -> IO c
animate = animate' 0 where
  animate' t f fps = do
    quitKey <- getKey $ SpecialKey ESC
    when (quitKey == Press) $ terminate >> exitSuccess
    f t
    updateDisplay
    t' <- advanceTime (const $ pure ()) t fps
    animate' t' f fps
