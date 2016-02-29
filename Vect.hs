{-# LANGUAGE TypeSynonymInstances #-}

module Vect where

import Control.Monad
import Data.List
import GHC.Exts

import Test.QuickCheck
import Debug.Trace

import ListExtra

type V a = [a]

instance Num a => Num (V a) where
  a + b = zipWith (+) a b
  negate = map negate
  fromInteger n = [fromInteger n]

  (*) = undefined
  abs = undefined
  signum = undefined

vToV2 [a,b] = V2 a b
v2ToV (V2 a b) = [a,b]
vToV3 [a,b,c] = V3 a b c
v3ToV (V3 a b c) = [a,b,c]

toV2 (x,y) = V2 x y

--TODO: move
v2ToV3 (V2 x y) = V3 x y 0

--TODO: replace with instance of Foldable class
v2ToList (V2 a b) = [a,b]

--Change GLR in GUI.hs too, if you change this
type R = Double
data V2 a = V2 a a deriving (Show, Eq, Ord)

type Angle a = a
type Distance a = a
type Point = V2

instance Arbitrary a => Arbitrary (V2 a) where
  arbitrary = liftM2 V2 arbitrary arbitrary
  shrink (V2 x y) = [V2 x' y | x' <- shrink x] ++
    [V2 x y' | y' <- shrink y]

--abs and signum return POLAR form vectors
--(*) takes POLAR form vectors and returns a RECT form one
instance (Floating a, RealFloat a) => Num (V2 a) where
  (V2 x y) + (V2 x' y') = V2 (x+x') (y+y')
  --TODO
  negate (V2 x y) = V2 (-x) (-y)

  --TODO: find better definition of (*)
  (V2 l a) * (V2 l' a') = fromPolar $ V2 (l*l') (a+a')
  abs v = V2 (len2 v) 0
  signum (V2 x y) = V2 1 (atan2 y x)

  fromInteger n = V2 (fromInteger n) 0

instance Functor V2 where
  fmap f (V2 x y) = V2 (f x) (f y)

len2 (V2 x y) = sqrt $ x*x + y*y

lensq2 v = v `dot2` v

dist2 a b = len2 (a-b)

distsq2 a b = lensq2 (a-b)

near2 dist a b = distsq2 a b < dist*dist

scale s = fmap (*s)

dot2 (V2 x y) (V2 x' y') = x*x' + y*y'

cross2 (V2 x y) (V2 x' y') = x*y' - y*x'

--TODO: these shouldn't take V2 as args
fromPolar (V2 l a) = V2 (l*cos a) (l*sin a)
toPolar v@(V2 x y) = V2 (len2 v) (atan2 y x)

makeLength2 len v = fmap (\a->a*len/len2 v) v
normalize2 = makeLength2 1

angleTowards a b = let (V2 x y) = b-a in atan2 y x

withinBounds2 (V2 x y) (V2 a b) (V2 a' b') =
  x>=a && x<=a' && y>=b && y<=b'

--TODO: finish 3D vectors

data V3 a = V3 a a a deriving (Show, Eq, Ord)

mkV3 :: (a,a,a) -> V3 a
mkV3 (x,y,z) = V3 x y z

mkV3' :: [a] -> V3 a
mkV3' [x,y,z] = V3 x y z

instance (Floating a, RealFloat a) => Num (V3 a) where
  (V3 x y z) + (V3 x' y' z') = V3 (x+x') (y+y') (z+z')
  negate (V3 x y z) = V3 (-x) (-y) (-z)

  (*) = undefined
  abs = undefined
  signum = undefined

  fromInteger n = V3 (fromInteger n) 0 0

instance Functor V3 where
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)

len3 (V3 x y z) = sqrt $ x*x + y*y + z*z

dist3 a b = len3 (a-b)

dot3 (V3 x y z) (V3 x' y' z') = x*x' + y*y' + z*z'

normalize3 v = fmap (/len3 v) v


--Axis-aligned bounding boxes

type AABB a = (V3 a, V3 a)

collidingOn :: (Ord a, Num a) => AABB a -> AABB a -> [Bool]
collidingOn (V3 ax ay az, V3 axs ays azs) (V3 bx by bz, V3 bxs bys bzs) =
  map bbTest [(ax,axs,bx,bxs),(ay,ays,by,bys),(az,azs,bz,bzs)]

dists :: (Ord a, Num a) => AABB a -> AABB a -> [a]
dists (V3 ax ay az, V3 axs ays azs) (V3 bx by bz, V3 bxs bys bzs) =
  map bbDist [(ax,axs,bx,bxs),(ay,ays,by,bys),(az,azs,bz,bzs)]

bbDist (a, as, b, bs) = abs (a-b) - as - bs
bbTest x = bbDist x < 0

bbColliding :: (Ord a, Num a) => AABB a -> AABB a -> Bool
bbColliding = and ... collidingOn

bbBounds :: Num a => AABB a -> (V3 a, V3 a)
bbBounds (V3 x y z,V3 x' y' z') = (V3 (x-x') (y-y') (z-z'), V3 (x+x') (y+y') (z+z'))

--TODO: fix this FUCKING PIECE OF SHIT
--It doesn't work!
resolveCollision :: (Ord a, Num a) => AABB a -> AABB a -> AABB a
resolveCollision a@(apos@(V3 ax ay az),asize@(V3 axs ays azs))
  b@(bpos@(V3 bx by bz),bsize@(V3 bxs bys bzs)) =
  if and colliding
    then (\x->(mkV3' x,asize)) $
      let zipped = zip6 d d' apos' asize' bpos' bsize' in
      for zipped $ \(d,d',a,as,b,bs)->
        if d'
          then if b>a then a+d else a-d
          else a
    else (apos,asize)
  where
    colliding = collidingOn a b
    d = dists a b
    d' = trueIfMax d
    f (a,b,c) = [a,b,c]
    f' [a,b,c] = (a,b,c)
    apos' = [ax,ay,az]
    asize' = [axs,ays,azs]
    bpos' = [bx,by,bz]
    bsize' = [bxs,bys,bzs]

trueIfMax :: Ord a => [a] -> [Bool]
trueIfMax xs = replaceAt x True (replicate (length xs) False)
  where
    x = fst $ last $ sortWith snd $ zip [0..] xs
