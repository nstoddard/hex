{-# LANGUAGE TupleSections, NoMonomorphismRestriction #-}

module ListExtra where

import Data.List
import Data.Maybe
import Control.Arrow
import GHC.Exts
import Data.Char
import Data.Tuple

-- |Like !!, but acts starting from the end of the list
(!>!) :: [a] -> Int -> a
l !>! i = reverse l !! i

-- |Appends to the end of a list
(+:) :: [a] -> a -> [a]
l +: x = l++[x]

indexInList :: Int -> [a] -> Bool
indexInList i l = low <= i && i < high
  where (low,high) = listBounds l

listBounds :: [a] -> (Int,Int)
listBounds = const 0 &&& length

--TODO: find better name
-- |Like !!, but safer
(!!?) :: [a] -> Int -> Maybe a
l !!? i = if indexInList i l then Just (l!!i) else Nothing

--This is the same as listToMaybe
maybeHead :: [a] -> Maybe a
maybeHead l = if null l then Nothing else Just (head l)

mergePairedList :: [(a,a)] -> [a]
mergePairedList = concatMap (\(a,b)->[a,b])

mergeLists :: [a] -> [a] -> [a]
mergeLists = mergePairedList ... zip

takeList :: [Int] -> [a] -> [a]
takeList = flip $ map . (!!)

for :: [a] -> (a->b) -> [b]
for = flip map

ffor :: Functor f => f a -> (a->b) -> f b
ffor = flip fmap

concatFor = flip concatMap

replaceAssoc :: Eq a => [(a,b)]->[(a,b)]->[(a,b)]
replaceAssoc list replacements = replacements ++
  filter (\(a,_)->and $ for replacements $ \(a',_)->a/=a') list

replaceSequence :: Eq a => [a] -> [a] -> [a] -> [a]
replaceSequence _ _ [] = []
replaceSequence a b xs@(x:xss)
  | a==take l xs = b ++ replaceSequence a b (drop l xs)
  | True = x : replaceSequence a b xss
  where
    l = length a

none :: (a->Bool) -> [a] -> Bool
none = not ... any

countBy :: (a->Bool) -> [a] -> Int
countBy = length ... filter

count :: Eq a => a -> [a] -> Int
count x = countBy (==x)

--These are unsafe; use them only when you're sure the element is in the
--list.
alwaysFind :: (a->Bool) -> [a] -> a
alwaysFindIndex :: (a->Bool) -> [a] -> Int
alwaysElemIndex :: Eq a => a -> [a] -> Int
alwaysLookup :: Eq a => a -> [(a,b)] -> b
alwaysFind f x = fromMaybe (error "alwaysFind failed")
  (find f x)
alwaysFindIndex f x = fromMaybe (error "alwaysFindIndex failed")
  (findIndex f x)
alwaysElemIndex a x = fromMaybe (error "alwaysElemIndex failed")
  (elemIndex a x)
alwaysLookup x l = fromMaybe (error "alwaysLookup failed")
  (lookup x l)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x l = take i l ++ [x] ++ drop (i+1) l

dropEnd :: Int -> [a] -> [a]
dropEnd i = reverse . drop i . reverse

takeEnd :: Int -> [a] -> [a]
takeEnd i = reverse . take i . reverse

sublist :: Eq a => [a] -> [a] -> [Int]
sublist l_ x_ = _sublist l_ x_ 0
  where
    _sublist [] _ _ = []
    _sublist l@(_:as) x i
      | take (length x) l == x = i:_sublist as x (i+1)
      | True = _sublist as x (i+1)

replaceSublists :: Eq a => [a] -> [a] -> [a] -> [[a]]
replaceSublists l a b = map (replaceAt_ l b (length a)) sub
  where
    sub = sublist l a

replaceAt_ :: (Num a, Eq a) => [t] -> [t] -> a -> a -> [t]
replaceAt_ l _ 0 _ = l
replaceAt_ (_:ls) [] n 0 = replaceAt_ ls [] (n-1) 0
replaceAt_ (_:ls) (b:bs) n 0 = b:replaceAt_ ls bs (n-1) 0
replaceAt_ (l:ls) b n i = l:replaceAt_ ls b n (i-1)
replaceAt_ _ _ _ _ = error "Can't call replaceAt_ with those args"

iterateN :: Int -> (a->a) -> a -> a
iterateN n = (!!n) ... iterate

--TODO: what is this for?
break' :: (a->Bool) -> [a] -> ([a],[a])
break' = second (drop 1) ... break

deleteIndex :: Int -> [a] -> [a]
deleteIndex _ [] = []
deleteIndex 0 (_:as) = as
deleteIndex n (a:as)
  | n < 0 = error "Invalid index in deleteIndex"
  | True = a:deleteIndex (n-1) as

deleteAssoc :: Eq a => a -> [(a,b)] -> [(a,b)]
deleteAssoc _ [] = []
deleteAssoc a ((a',b):xs)
  | a==a' = xs
  | True = (a',b):deleteAssoc a xs

--- Splitting functions ---

-- |SplitInto n xs divides xs into groups of n elems each
splitInto :: Int -> [a] -> [[a]]
splitInto = splitInto' . repeat

-- |A generalized version of splitInto
splitInto' :: [Int] -> [a] -> [[a]]
splitInto' _ [] = []
splitInto' (n:ns) l = take n l : splitInto' ns (drop n l)
splitInto' [] xs = [xs]

-- |Splits a list at a certain value, like a generalized version of
-- "lines" or "words"
-- The opposite of intersperse
splitList :: Eq a => a -> [a] -> [[a]]
splitList x = filter (not . null) . map (filter (/=x)) . splitList2 x

-- |Like splitList, but keep the characters
splitList2 :: Eq a => a -> [a] -> [[a]]
splitList2 = filter (not.null) ... splitList2' [] where
  splitList2' cur _ [] = [cur]
  splitList2' cur x (y:ys)
    | x==y = cur : [x] : splitList2' [] x ys
    | True = splitList2' (cur+:y) x ys

splitOn :: Eq a => a -> [a] -> ([a],[a])
splitOn x xs
  | null res = error "No res"
  | length res==2 = (head res,res!!1)
  | length res==1 = error "Too few res"
  | length res>2 = error "Too many res"
  where
    res = splitList x xs

splitOnLast :: Eq a => a -> [a] -> ([a],[a])
splitOnLast x xs
  | null res = error "No res"
  | length res==1 = error "Too few res"
  | True = (res!!(length res-2),res!!(length res-1))
  where
    res = splitList x xs

replaceIf :: (a->Bool) -> a -> [a] -> [a]
replaceIf f y = map (\x -> if f x then y else x)

replace :: Eq a => a -> a -> [a] -> [a]
replace a = replaceIf (==a)

-- |Like nub, but only removes duplicate adjacent elements
-- Example: nubAdjacent [1,2,2,1,2] = [1,2,1,2]
nubAdjacent :: Eq a => [a] -> [a]
nubAdjacent = map the . group

{---TODO: move to real libs
nubAdjacentWith :: (Eq a, Ord b) => (a -> b) -> [a] -> [a]
nubAdjacentWith f = map the . groupWith f-}

--TODO: generalize
findDiffAdjacentWith :: Eq b => (a -> b) -> [a] -> [(a,a)]
findDiffAdjacentWith _ [] = []
findDiffAdjacentWith _ [_] = []
findDiffAdjacentWith f (a:b:xs)
  | f a == f b = findDiffAdjacentWith f (b:xs)
  | True = (a,b) : findDiffAdjacentWith f (b:xs)

--TODO: simplify these two functions
--TODO: add to real libs
--The function should return true if the value goes in a new list
--TODO: change so that the function returns true if the value stays in the same list
groupAdjacentBy :: (a -> Bool) -> [a] -> [[a]]
groupAdjacentBy = map reverse ... filter (not . null) ...
  groupAdjacentBy' [] where
  groupAdjacentBy' cur _ [] = [cur]
  groupAdjacentBy' cur f (x:xs)
    | f x = cur : groupAdjacentBy' [x] f xs
    | True = groupAdjacentBy' (x:cur) f xs

--groupBy doesn't do what I want it to
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' = map reverse ... filter (not . null) ... groupBy'_ [] where
  groupBy'_ cur _ [] = [cur]
  groupBy'_ [] f (x:xs) = groupBy'_ [x] f xs
  groupBy'_ (cur:curs) f (x:xs)
    | f cur x = groupBy'_ (x:cur:curs) f xs
    | True = (cur : curs) : groupBy'_ [x] f xs

--This should have been equivalent to groupBy
{---TODO: make sure this works properly
--TODO: add to real libs
groupAdjacentWith :: (a -> a -> Bool) -> [a] -> [[a]]
groupAdjacentWith = map reverse ... filter (not . null) ...
  groupAdjacentWith' [] where
  groupAdjacentWith' cur _ [] = [cur]
  groupAdjacentWith' cur _ [x] = [x:cur]
  groupAdjacentWith' cur f (x:y:xs)
    | f x y = cur : groupAdjacentWith' [x] f (y:xs)
    | True = groupAdjacentWith' (x:cur) f (y:xs)-}

replaceAll as b xs = foldr (`replace` b) xs as

canLookup :: Eq a => a -> [(a,b)] -> Bool
canLookup = isJust ... lookup

lookup' :: Eq a => a -> [(a,b)] -> Maybe (a,b)
lookup' x xs = maybe
  Nothing
  (Just . (x,))
  (lookup x xs)

--TODO: move this to another file
--I wish I could give this precedence 10
infixr 9 ...
a ... b = (a .) . b

mSum :: Num a => (x->a) -> [x] -> a
mSum = sum ... map


capFirst :: String -> String
capFirst str = toUpper (head str) : tail str

sentence :: String -> String
sentence str = capFirst str ++ "."

--Pad a string with spaces to make it at least a certain length.
padL :: Int -> String -> String
padL = padWithL ' '
padR :: Int -> String -> String
padR = padWithR ' '

padWithL :: x -> Int -> [x] -> [x]
padWithL x n xs = replicate (n-length xs) x ++ xs
padWithR :: x -> Int -> [x] -> [x]
padWithR x n xs = xs ++ replicate (n-length xs) x


showAsTuple :: [String] -> String
showAsTuple x = "(" ++ intercalate ", " x ++ ")"

formatList :: String -> [String] -> String
formatList = formatList' False where
  formatList' _ _ [] = ""
  formatList' _ _ [a] = a
  formatList' comma op [a,b] = a ++ (if comma then ", " else " ") ++ op ++ " " ++ b
  formatList' _ op (x:xs) = x ++ ", " ++ formatList' True op xs

--iterate does about the same thing as this, but it's more general
sequ :: (a->b->a) -> a -> b -> [a]
sequ f a0 d = a0:sequ f (f a0 d) d

cond :: [(Bool, b)] -> b
cond [] = error "All causes of cond failed."
cond ((f, b):xs) = if f then b else cond xs

cond' :: [(a->Bool, b)] -> a -> b
cond' [] _ = error "All clauses of cond' failed."
cond' ((f, b):xs) a = if f a then b else cond' xs a

cond'' :: Eq a => b -> a -> [(a,b)] -> b
cond'' def = fromMaybe def ... lookup

-- |Like map, but allow the function to access the index too
mapInd :: ((Int,a)->b) -> [a] -> [b]
mapInd f = map f . addIndices

addIndices :: [a] -> [(Int,a)]
addIndices = zip [0..]

--TODO: reverseLookup'
reverseLookup :: Eq b => b -> [(a,b)] -> Maybe a
reverseLookup a = lookup a . map swap
