module NumExtra where

import Data.Fixed (mod')
import Data.List
import Control.Applicative

import ListExtra
import MiscExtra

tau :: Floating t => t
tau = 2*pi

radToDeg :: Floating t => t -> t
radToDeg rad = rad*360/tau

--mod is more useful if the result is positive
posmod :: Integral a => a -> a -> a
x `posmod` y = abs (x `mod` y)

x `posmod'` y = abs (x `mod'` y)

average l = sum l / fromIntegral (length l)

compareMod :: Integral a => a -> a -> a
compareMod diff base =
  if diff' >= base `quot` 2 then diff'-base else diff'
  where diff' = diff `posmod` base

-- Rounds to a certain number of decimal places
-- (Doesn't work for displaying rounded decimals:
-- roundN 1 1.2 results in 1.2000000000000002)
roundN :: (RealFrac a, Floating a) => a -> a -> a
roundN n x = rounded * 10**(-n)
  where rounded = realToFrac $ roundUp (x*10**n)

lerp :: Fractional a => (a,a) -> (a,a) -> a -> a
lerp (x0,x1) (y0,y1) x = y0 + (x-x0)*(y1-y0)/(x1-x0)

lerp' :: (Real a,Fractional b) => (a,a) -> (a,a) -> a -> b
lerp' (x0,x1) (y0,y1) x = lerp (rtf x0,rtf x1) (rtf y0,rtf y1) (rtf x)
  where rtf = realToFrac

quad :: (Floating a, Eq a) => a -> a -> a -> [a]
quad 0 b c = [-c/b]
quad a b c = [(-b+x)/(2*a),(-b-x)/(2*a)] where
  x = sqrt (b*b-4*a*c)

-- |Like round, but rounds 0.5 up rather than to the nearest even integer
roundUp :: (RealFrac a, Integral b) => a -> b
roundUp x = case signum (r' - 0.5) of
  -1 -> n
  0 -> if n<=0 then m+1 else m
  1 -> m
  _ -> error "Invalid case option in roundUp"
  where
    (n,r) = properFraction x
    r' = abs r
    m = if r<0 then n-1 else n+1

divideTowards :: Fractional a => a -> a -> a -> a
divideTowards attractor x y = attractor-((attractor-x)/y)


arith :: Num a => a -> a -> [a]
geom :: Num a => a -> a -> [a]
arith = sequ (+)
geom = sequ (*)


close :: (Ord a, Num a) => a -> a -> a -> Bool
close epsilon a b = abs (a-b) < epsilon

close' :: (Ord a, Fractional a) => a -> a -> Bool
close' = close 1.0e-5


limit :: Ord a => a -> a -> a -> a
limit a b = max a . min b


--From the Haskell wiki
--Modified to always put the points in order
line :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
line pa@(xa,ya) pb@(xb,yb) =
  if head result==pa then result else reverse result
  where
    result = map maySwitch . unfoldr go $ (x1,y1,0)
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch = if steep then (\(x,y) -> (y,x)) else id
    [(x1,y1),(x2,y2)] = sort [maySwitch pa, maySwitch pb]
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, err)
        | xTemp > x2 = Nothing
        | otherwise  = Just ((xTemp, yTemp), (xTemp + 1, newY, newError))
        where
          tempError = err + deltay
          (newY, newError) = if (2*tempError) >= deltax
                            then (yTemp+ystep,tempError-deltax)
                            else (yTemp,tempError)

--- Numeric functions/2D Vectors (should remove) ---

vadd :: Num a => (a, a) -> (a, a) -> (a, a)
vadd = pair2 (+)
vsub :: Num a => (a, a) -> (a, a) -> (a, a)
vsub = pair2 (-)

vleq :: Ord a => (a,a) -> (a,a) -> Bool
vleq (x,y) (x', y') = x<=x' && y<=y'

pdiff :: Num a => (a,a) -> a
pdiff (a,b) = a-b

vsmul :: Num a => (a, a) -> a -> (a, a)
vsmul a b = pair1 (*b) a

vzero :: Num a => (a, a)
vzero = (0, 0)

range_ a b step = take (fromIntegral b) $ arith a step
range2 a b = range_ a b 1
prange = uncurry range2

rect (x0, y0) (x1, y1) = (,) <$> range2 x0 x1 <*> range2 y0 y1
rect_ (x0, y0) (x1, y1) (xs, ys) =
  (,) <$> range_ x0 x1 xs <*> range_ y0 y1 ys
rect2 (x0,y0) (x1,y1) = (,) <$> [x0..x1] <*> [y0..y1]
--end things to remove
