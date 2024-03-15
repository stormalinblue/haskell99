module Problems31To41 (
    isPrime,
    myGCD,
    coprime,
    totient,
    primeFactors,
    primeFactorsMult,
    phi,
    primesR,
    goldbach,
    goldbachList,
    goldbachList'
) where

-- Problem 31
divides :: Int -> Int -> Bool
divides a b = b `rem` a == 0

isPrime :: Int -> Bool
isPrime n
    | n == 1 = False
    | n == 2 = True
    | otherwise = let
    in not (any (`divides` n) $ takeWhile (\x -> x * x <= n) [2..])

-- Problem 32
myGCD :: Int -> Int -> Int
myGCD a b
    | a == 1 = 1
    | a > b = gcd b (a `rem` b)
    | otherwise = gcd b a

-- Problem 33
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

-- Problem 34
totient :: Int -> Int
totient n = length $ filter (coprime n) [1..n-1]

-- Problem 35
primeFactors :: Int -> [Int]
primeFactors n =
    let 
        primeFactorsInner a prev
            | a == prev = [prev]
            | prev `divides` a = prev:primeFactorsInner (a `div` prev) prev
            | otherwise = primeFactorsInner a (prev + 1)
    in primeFactorsInner n 2

-- Problem 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n =
    let
        factors = primeFactors n
        rle :: Eq a => [a] -> [(Int, a)]
        rle [] = []
        rle (x:xs) = let
            (c, rest) = break (/= x) xs
            in (1 + length c, x):rle rest
    in rle factors

-- Problem 37
phi :: Int -> Int
phi n =
    product [(p - 1) * p ^ (m - 1) | (m, p) <- primeFactorsMult n]

-- Problem 38
primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a..b]

-- Problem 39
goldbach :: Int -> (Int, Int)
goldbach n = let
    primes = filter isPrime [2..]
    in head [(p, n - p) | p <- primes, p < n, isPrime (n - p)]

-- Problem 40
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b
    | odd a = goldbachList (a + 1) b
    | otherwise = map goldbach [a, a + 2..b]

-- Problem 41
goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b thresh =
    filter (\(x, y) -> (x > thresh) && (y > thresh)) (goldbachList a b)