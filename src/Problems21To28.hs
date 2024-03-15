module Problems21To28 (
    insertAt,
    range,
    rndSelect,
    diffSelect,
    rndPermu,
    combinations,
    groups,
    lsort,
    lfsort
) where

import qualified Data.Map as Map
import System.Random ( newStdGen, Random(randomR), RandomGen )
import Data.List (sortOn)

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt a [] _ = [a]
insertAt a x 0 = a:x
insertAt a x 1 = a:x
insertAt a (x:xs) n = x:insertAt a xs (n - 1)

-- Problem 22
range :: Int -> Int -> [Int]
range a b
    | a <= b = a:range (a + 1) b
    | otherwise = []

-- Problem 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect list numEl = let
    rndSelectInner :: RandomGen g => Map.Map Int e -> Int -> g -> [e]
    rndSelectInner m sz gen
        | sz <= 0 = []
        | otherwise = let
            (extractIndex, newGen) = randomR (0, sz - 1) gen
            res = m Map.! extractIndex
            afterUpdate = Map.insert extractIndex (m Map.! (sz - 1)) m
        in res:rndSelectInner afterUpdate (sz - 1) newGen
    in do
        gen <- newStdGen
        let sz = length list
        let m = Map.fromList $ zip [0..] list
        return (take numEl $ rndSelectInner m sz gen)

-- Problem 24
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = rndSelect [1..m] n

-- Problem 25
rndPermu :: [a] -> IO [a]
rndPermu l = rndSelect l (length l)

-- Problem 26
combinations :: [a] -> Int -> [[a]]
combinations list total = let
        merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) ys = x:merge ys xs

        combinationsInner :: Int -> [[a]] -> [a] -> [[a]]
        combinationsInner 0 prev _ = prev
        combinationsInner _ _ [] = []
        combinationsInner budget prev (x:xs) = let
            withHead = combinationsInner (budget - 1) [x:prevItem | prevItem <- prev] xs
            withoutHead = combinationsInner budget prev xs
            in merge withHead withoutHead
    in combinationsInner total [[]] list

-- Problem 27
groups :: [Int] -> [a] -> [[[a]]]
groups budgets list =
    let
        flatten :: [[a]] -> [a]
        flatten [] = []
        flatten (x:xs) = x ++ flatten xs

        prefixPossibilities :: letter -> [[letter]] -> [[[letter]]]
        prefixPossibilities _ [] = []
        prefixPossibilities letter [[]] = [[[letter]]]
        prefixPossibilities letter ((word:otherWords)) = ((letter:word):otherWords):[word:o | o <- prefixPossibilities letter otherWords]

        subtract1 :: [Int] -> [[Int]]
        subtract1 [] = []
        subtract1 (x:xs) = ((x - 1):xs):[x:o | o <- subtract1 xs]

        step :: [l] -> [([Int], [[l]])] -> [([Int], [[l]])]
        step [] wordBudgets = wordBudgets
        step (letter:rletters) budgetsAndSuffixes = let
                budgetSatisfied :: ([Int], words) -> Bool
                budgetSatisfied (b, _) = all (>= 0) b

                nextPhase =  flatten [filter budgetSatisfied (zip (subtract1 sBudgets) (prefixPossibilities letter suffixes)) | (sBudgets, suffixes) <- budgetsAndSuffixes ]
            in step rletters nextPhase
    in map snd (step list [(budgets, [[] | _ <- budgets])])

-- Problem 28 (a)
lsort :: [[a]] -> [[a]]
lsort = sortOn length

-- Problem 29 (b)
lfsort :: [[a]] -> [[a]]
lfsort l = let
    updater :: Maybe Int -> Maybe Int
    updater x = case x of
        Just a -> Just (a + 1)
        Nothing -> Just 1
    m :: Map.Map Int Int
    m = foldr (Map.alter updater . length) Map.empty l
    in sortOn (\x -> m Map.! length x) l