module RandomExtra where

import System.Random
import Control.Monad
import ListExtra
import Control.Applicative

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen = (value:restOfList, finalGen)
  where
    (value, newGen) = random gen
    (restOfList, finalGen) = finiteRandoms (n-1) newGen


finiteRandomRs :: (RandomGen g, Random a, Num n, Eq n) => n -> (a, a) -> g -> ([a], g)
finiteRandomRs 0 _ gen = ([], gen)
finiteRandomRs n range gen = (value:restOfList, finalGen)
  where
    (value, newGen) = randomR range gen
    (restOfList, finalGen) = finiteRandomRs (n-1) range newGen

--TODO: generalize these to non-IO-based random numbers

-- |Returns a random index into the list
randomIndexIO :: [a] -> IO Int
randomIndexIO l = randomRIO (0,length l-1)

randomIndex :: RandomGen g => g -> [a] -> (Int, g)
randomIndex g l = randomR (0,length l-1) g

-- |Returns a random element of the list
randomElemIO :: [a] -> IO a
randomElemIO l = liftM (l!!) $ randomIndexIO l

-- |chance n d has a n/d chance of returning True
chanceIO :: Int -> Int -> IO Bool
chanceIO n d = do
  val <- randomRIO (0, d-1)
  pure (val<n)

-- |like chance, but takes a probability
chanceIO' :: Double -> IO Bool
chanceIO' x = do
  val <- randomIO
  pure (val<x)

-- |Randomly shuffles a list
shuffleIO :: [a] -> IO [a]
shuffleIO [] = pure []
shuffleIO xs = do
  i <- randomIndexIO xs
  xs' <- shuffleIO (deleteIndex i xs)
  pure $ xs!!i : xs'

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle _ [] = []
shuffle g xs = xs!!i : xs'
  where
    (i,g') = randomIndex g xs
    xs' = shuffle g' (deleteIndex i xs)
