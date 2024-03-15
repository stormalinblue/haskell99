module Problems1To10 (
    myLast,
    myButLast,
    elementAt,
    myLength,
    myReverse,
    isPalindrome,
    flatten,
    compress,
    pack,
    NestedList(Elem, List),
    encode) where

-- Problem 1
myLast :: [a] -> a
myLast [] = error "Must be a finite list"
myLast [a] = a
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "Must be a list of length >= 2"
myButLast [_] = error "Must be a list of length >= 2"
myButLast [x,_] = x
myButLast (_:rest@(_:_)) = myButLast rest

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Out of bounds"
elementAt (x:_) 0 = x
elementAt (_:xs) a
    | a < 0 = error "Out of bounds"
    | otherwise = elementAt xs (a - 1)

-- Problem 4
myLength :: [a] -> Int
myLength x =
    myLengthInner x 0 where
        myLengthInner :: [a] -> Int -> Int
        myLengthInner [] a = a
        myLengthInner (_:ys) a = myLengthInner ys (a + 1)

-- Problem 5
myReverse :: [a] -> [a]
myReverse x =
    let
        myReverseInner :: [a] -> [a] -> [a]
        myReverseInner [] a = a
        myReverseInner [a] y = a:y
        myReverseInner (a:as) y = myReverseInner as (a:y)
    in myReverseInner x []

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = a == myReverse a

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten x = let
    flattenInner :: NestedList a -> [a] -> [a]
    flattenInner (Elem a) y = y ++ [a]
    flattenInner (List []) y = y
    flattenInner (List [a]) y = y ++ flattenInner a []
    flattenInner (List (a:as)) y = flattenInner (List as) (y ++ flattenInner a [])
    in flattenInner x []

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) =
    let
        compressInner :: Eq a => [a] -> a -> [a] -> [a]
        compressInner [] _ revRes = revRes 
        compressInner (a:as) y revRes
            | y == a = compressInner as y revRes
            | otherwise = compressInner as a (a:revRes)
    in myReverse $ compressInner xs x [x]


-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack x =
    let
        collectedCommon :: Eq a => [a] -> a -> [a] -> ([a], [a])
        collectedCommon [] _ prefix = (myReverse prefix, []) 
        collectedCommon string@(a:as) alpha prefix
            | a == alpha = collectedCommon as alpha (a:prefix)
            | otherwise = (myReverse prefix, string)

        packInner :: Eq a => [a] -> [[a]] -> [[a]]
        packInner [] result = myReverse result
        packInner (a:as) result = let
            (prefix, suffix) = collectedCommon as a []
            in packInner suffix ((a:prefix):result)
    in packInner x []

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode x = [(length y, head y) | y <- pack x]