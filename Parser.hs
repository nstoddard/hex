{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser where

--A toy parser combinator implementation; probably not suitable for
--real applications

--TODO:
-- error messages
-- monad transformer

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Char

import MonadExtra

newtype Parser s a = Parser {parseFn :: ([s] -> [(a,[s])])}

--Filter all possible parses to include only those that parse the entire input
--If no results are returned, there was a parse error (or the entire input wasn't parsed)
--If more than one result is returned, the parser is ambiguous
parse = ((map fst . filter (null.snd)) .) . parseFn

instance Monad (Parser s) where
  return a = Parser $ \input-> [(a,input)]
  (Parser a) >>= b = Parser $
    concatMap (\(val,str)->parseFn (b val) str) . a

instance Functor (Parser s) where
  fmap = liftM

instance Applicative (Parser s) where
  pure = return
  (<*>) = ap

--Monoid, Alternative, and MonadPlus seem to be basically the same
--typeclass. Monoid is the most fundamental one. Alternative is
--Applicative+Monoid; MonadPlus is Monad+Monoid
instance Monoid (Parser s a) where
  mempty = Parser (const [])
  mappend a b = Parser $ \s->parseFn a s ++ parseFn b s

instance Alternative (Parser s) where
  empty = mempty
  (<|>) = mappend

instance MonadPlus (Parser s) where
  mzero = mempty
  mplus = mappend

--Applies a, then b, and combines the results in a pair
(<+>) :: Monad m => m a -> m b -> m (a,b)
(<+>) = zipM

-- |Applies a and b to the same input, and returns the result only if they
-- both parse the same thing
(<&>) :: (Eq s,Eq a) => Parser s a -> Parser s a -> Parser s a
a <&> b = Parser f
  where
    f input = if a'==b' then a' else []
      where
        a' = parseFn a input
        b' = parseFn b input

satisfy :: (a->Bool) -> Parser a a
satisfy fn = Parser f
  where
    f input
      | null input = []
      | fn (head input) = [(head input,tail input)]
      | True = []

term x = satisfy (==x)
terms = mapM term

--Like term and terms, but case-insensitive
--Return the term in lowercase
term' x = liftM toLower $ satisfy (`elem` [toLower x, toUpper x])
terms' = mapM term'

anyTerm = satisfy (const True)
digit = satisfy isDigit
letter = satisfy isLetter
whitespace = satisfy isSpace
oneOf xs = satisfy (`elem`xs)
noneOf xs = satisfy (`notElem`xs)

--Case-insensitive
oneOf' xs = satisfy (`elem`(map toLower xs ++ map toUpper xs))
noneOf' xs = satisfy (`notElem`(map toLower xs ++ map toUpper xs))

sepBy1 :: (Alternative f, Monad f) => f a -> f b -> f [a]
sepBy1 p sep = fmap return p <|> do
  a <- p
  sep
  as <- sepBy1 p sep
  return (a:as)

sepBy :: (Alternative f, Monad f) => f a -> f b -> f [a]
sepBy p sep = return [] <|> sepBy1 p sep
