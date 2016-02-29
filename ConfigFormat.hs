{-
  A relatively simple config file parser.
-}

{- Config file format
  Basic data types
    Nil
    Int - a series of digits without a decimal point
    Float - a series of digits with a decimal point
    String - surrounded by quotes
    Bool - true or false
    --TODO: Char?
  Composite data types
    List - surrounded by [] or ()   -- TODO: remove [] around lists
    Data constructor - starts with any capital letter
    Var - has a name and a value
  Syntax
    The file must have a newline at the end.
-}

{-module ConfigFormat (
  Config, readConfig,
  getInt, getFloat, getStr, getBool, getList, getData,
  getIntOpt, getFloatOpt, getStrOpt, getBoolOpt, getListOpt, getDataOpt
) where-}
module ConfigFormat where

import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad
import Control.Applicative
import Data.Char
import Data.Maybe
import System.Directory
import Debug.Trace

import Parser
import ListExtra
import MonadExtra

type Config = Expr

readConfig :: FilePath -> IO (Maybe Config)
readConfig file = do
  exists <- doesFileExist file
  if not exists then pure Nothing else do
  --unless exists $ error $ "Config file " ++ show file ++ " doesn't exist!"
  str <- readFile file
  case parse (liftM List $ many parseLine) str of
    [] -> error $ "Parse error while parsing " ++ file
    [expr] -> pure (Just expr)
    xs -> error $ "Ambiguous config file: " ++ unlines (map show xs)

getInt :: Config -> Int
getInt x = case x of
  Int_ x -> x
  _ -> error $ "Wrong type for " ++ show x

getFloat x = case x of
  Float_ x -> x
  Int_ x -> fromIntegral x
  _ -> error $ "Wrong type for " ++ show x

getStr x = case x of
  Str x -> x
  _ -> error $ "Wrong type for " ++ show x

getBool x = case x of
  Boolean x -> x
  _ -> error $ "Wrong type for " ++ show x

getList x = case x of
  List x -> x
  _ -> error $ "Wrong type for " ++ show x

getData x = case x of
  Data x xs -> (x,xs)
  _ -> error $ "Wrong type for " ++ show x


getIntOpt :: Config -> String -> Int
getIntOpt = getInt ... getVar

getFloatOpt :: Config -> String -> Double
getFloatOpt = getFloat ... getVar

getStrOpt :: Config -> String -> String
getStrOpt = getStr ... getVar

getBoolOpt :: Config -> String -> Bool
getBoolOpt = getBool ... getVar

getListOpt :: Config -> String -> [Config]
getListOpt = getList ... getVar

getDataOpt :: Config -> String -> (String,[Config])
getDataOpt = getData ... getVar


getVar (List xs) x = case find (isVar x) xs of
  Nothing -> error $ "Can't find option " ++ x
  Just (Var _ val) -> val
  _ -> error $ "Internal error in getVar"
getVar _ _ = error "getVar: not list"

isVar x (Var x' _) = x == x'
isVar _ _ = False


data Expr = Nil | Int_ Int | Float_ Double | Str String |
  Identifier String | Boolean Bool |
  List [Expr] | Var String Expr | Data String [Expr]
  deriving (Show)

spaces = many whitespace

parseLine = parseExpr >>! many (satisfy $ \c -> isSpace c && c /= '\n') >>! term '\n'

parseExpr = spaces >> (parseParens <|> parseList '[' ']' <|> parseList '(' ')' <|>
  parseInt <|> parseFloat <|> parseString <|> parseId <|> parseAssign{- <|> parseData-})

parseMinus = (term '-' >> pure True) <|> pure False

parseInt = do
  minus <- parseMinus
  digits <- some digit
  pure (Int_ $ (if minus then negate else id) $ read digits)

parseFloat = do
  minus <- parseMinus
  digits' <- many digit
  let digits = if null digits' then "0" else digits'
  term '.'
  decimal' <- many digit
  let decimal = if null decimal' then "0" else decimal'
  pure (Float_ $ (if minus then negate else id) $ read $ digits ++ "." ++ decimal)


escapeChars = [('n','\n'), ('t','\t'), ('\\','\\'), ('0','\0'),
  ('b','\b'), ('r','\r'), ('"','"'), ('\'','\'')]

parseEscapeChar = do
  term '\\'
  x <- oneOf (map fst escapeChars)
  pure . fromJust $ lookup x escapeChars

parseString = do
  term '"'
  chars <- many (parseEscapeChar <|> noneOf "\"")
  term '"'
  pure $ Str chars

parseId = do
  let
    fstCheck = satisfy isLower
    restCheck = letter
  fst <- fstCheck
  rest <- many restCheck
  let id = fst:rest
  case lookup id defaultEnv of
    Just val -> pure val
    Nothing -> pure $ Identifier id

parseParens = do
  term '('
  x <- parseExpr
  spaces >> term ')'
  pure x

parseList start end = do
  term start
  xs <- sepBy parseExpr (spaces >> term ',')
  spaces >> term end
  pure (List xs)

parseAssign = do
  id <- parseId
  case id of
    Identifier name -> do
      spaces >> term '='
      val <- parseExpr
      pure (Var name val)
    _ -> empty

{-parseDataName = do
  let
    fstCheck = satisfy isUpper
    restCheck = letter
  fst <- fstCheck
  rest <- many restCheck
  traceShow (fst,rest) $ pure (fst:rest)

parseData = do
  name <- parseDataName
  whitespace
  vals <- many parseExpr
  pure (Data name vals)-}

type Env = Map String Expr

defaultEnv = [("nil", Nil), ("true", Boolean True), ("false", Boolean False)]
