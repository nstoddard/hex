{-# LANGUAGE NoMonomorphismRestriction #-}

module MonadExtra where

import Control.Monad
import Control.Applicative
import Data.Maybe

import ListExtra

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM = liftM join ... mapM

concatForM = flip concatMapM

findM :: Monad m => (a->m Bool) -> [a] -> m (Maybe a)
findM f xs = liftM listToMaybe $ filterM f xs

zipM :: Monad m => m a -> m b -> m (a,b)
zipM = liftM2 (,)

when' :: (Monad m, MonadPlus n) => Bool -> m (n a) -> m (n a)
when' cond val = if cond then val else return mzero

unless' :: (Monad m, MonadPlus n) => Bool -> m (n a) -> m (n a)
unless' cond val = if not cond then val else return mzero

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ [] = return []
takeWhileM f (x:xs) = do
  res <- f x
  if res then do
    res2 <- takeWhileM f xs
    return (x:res2)
    else return []

takeWhileM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM' _ [] = return []
takeWhileM' f (x:xs) = do
  res <- f x
  if res then do
    res2 <- takeWhileM' f xs
    return (x:res2)
    else return [x]

repeatUntil :: Monad m => (a->Bool) -> m a -> m a
repeatUntil f m = do
  res <- m
  if f res then return res else repeatUntil f m

repeatWhile :: Monad m => (a->Bool) -> m a -> m a
repeatWhile f = repeatUntil (not.f)

(<<) :: Monad m => m a -> m b -> m a
a << b = b >> a

--Like >> and <<, but apply the monads in the opposite order
(>>!) :: Monad m => m a -> m b -> m a
a >>! b = do
  res <- a
  b
  return res

(!<<) :: Monad m => m a -> m b -> m b
a !<< b = b >>! a

while cond f = do
  res <- cond
  if res then f >> while cond f else pure ()

until cond = while (liftM not cond)

foldM1 :: Monad m => (a->a->m a) -> [a] -> m a
foldM1 f (x:xs) = foldM f x xs
foldM1 _ _ = error "Can't foldM1 on an empty list!"
