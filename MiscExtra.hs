{-# LANGUAGE NoMonomorphismRestriction #-}

module MiscExtra where

import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace
import Data.List
import Control.Applicative
import Control.Arrow
import GHC.Exts (sortWith)
import Text.Printf
import Control.Concurrent.MVar
import Control.Monad

import ListExtra

pairOf :: a -> (a,a)
pairOf a = (a,a)

pair1 :: (a->b) -> (a,a) -> (b,b)
pair1 fn (a, b) = (fn a, fn b)

pair2 :: (a->b->c) -> (a,a) -> (b,b) -> (c,c)
pair2 fn (a, b) (c, d) = (fn a c, fn b d)

findMin :: Ord b => (a->b) -> [a] -> a
findMin f = snd . head . sortWith fst . map (f&&&id)

findMax :: Ord b => (a->b) -> [a] -> a
findMax f = snd . last . sortWith fst . map (f&&&id)

-- TODO: make sure Integral and RealFrac are sufficient to prevent
-- exceptions from printf (which is type-unsafe)
showRounded :: (Integral a,Show a,RealFrac b,PrintfArg b) => a -> b -> String
showRounded n = printf ("%." ++ show n ++ "f")

allBound :: (Bounded a, Enum a) => [a]
allBound = [minBound .. maxBound]

traceShow' a b = traceShow (a,b) b

safeToEnum = enumIfBetween minBound maxBound

enumIfBetween :: Enum a => a -> a -> Int -> Maybe a
enumIfBetween a z x = if a'<=x && x<=z' then Just (toEnum x) else Nothing
  where
    a' = fromEnum a
    z' = fromEnum z

mapRename :: Ord k => k -> k -> Map k a -> Map k a
mapRename a b m = let val = M.lookup a m in case val of
  Nothing -> m
  Just val' -> M.insert b val' $ M.delete a m

--TODO: remove this
forceMVar :: MVar a -> a -> IO ()
forceMVar mvar a = do
  empty <- isEmptyMVar mvar
  unless empty $ void (takeMVar mvar)
  putMVar mvar a

toMaybe :: Bool -> a -> Maybe a
toMaybe True = Just
toMaybe False = const Nothing
