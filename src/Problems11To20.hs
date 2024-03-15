module Problems11To20 (
    EncodedPart(Multiple, Single),
    encode,
    decodeModified,
    encodeDirect,
    dupli,
    repli,
    dropEvery,
    split,
    slice,
    rotate,
    removeAt
) where

import Problems1To10 (pack)

-- Problem 11
data EncodedPart a = Multiple Int a | Single a deriving Show
encode :: Eq a => [a] -> [EncodedPart a]
encode x = [
    if n == 1 then Single alpha else Multiple n alpha
    | run <- pack x, let n = length run, let alpha = head run]


-- Problem 12
decodeModified :: [EncodedPart a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = case x of
    Single a -> a:decodeModified xs
    Multiple n a -> replicate n a ++ decodeModified xs

-- Problem 13
encodeDirect :: Eq a => [a] -> [EncodedPart a]
encodeDirect [] = []
encodeDirect (alpha:xs) = let
    runLength = 1 + length ( takeWhile (== alpha) xs )
    rest = dropWhile (== alpha) xs
    in (if runLength == 1 then Single alpha else Multiple runLength alpha):encodeDirect rest

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n =
    let
        repliInner :: a -> Int -> [a] -> [a]
        repliInner _ 0 fn = fn
        repliInner alpha i fn = alpha:repliInner alpha (i - 1) fn
    in repliInner x n (repli xs n)

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery x n = [e | (index, e) <- zip [1..] x, index `mod` n /= 0]

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split x n = let
    myReverse :: [a] -> [a] -> [a]
    myReverse [] res = res
    myReverse (a:as) res = myReverse as (a:res)

    splitInner :: [a] -> Int -> [a] -> ([a], [a])
    splitInner revFst 0 rest = (myReverse revFst [], rest)
    splitInner _ _ [] = error "out of bounds"
    splitInner revFst i (a:as) = splitInner (a:revFst) (i - 1) as
    in splitInner [] n x

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice x i j = (take (j - i + 1) . drop (max 0 (i - 1))) x

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate x n
    | n >= 0 = drop n x ++ take n x
    | otherwise = let
        l = length x
        in drop (l + n) x ++ take (l + n) x

-- Problem 20
removeAt :: [a] -> Int -> (a, [a])
removeAt x n = (x !! n, take n x ++ drop (n + 1) x)