module Problems54ATo60 (
    Tree(Empty, Branch),
    leaf,
    isTree,
    cbalTree,
    mirror,
    symmetric,
    construct,
    symCbalTrees,
    hbalTree,
    minNodes,
    maxHeight
) where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving Show

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-- Problem 54 A
isTree :: Tree a -> Bool
isTree _ = True

-- Problem 55
cbalTree :: Int -> [Tree Char]
cbalTree 1 = [leaf 'x']
cbalTree 2 = [Branch 'x' (leaf 'x') Empty, Branch 'x' Empty (leaf 'x')]
cbalTree n
    | odd n = let
        halfNodes = (n - 1) `div` 2
        cbalTreeWithHalfNodes = cbalTree halfNodes
        in [Branch 'x' t1 t2 | t1 <- cbalTreeWithHalfNodes, t2 <- cbalTreeWithHalfNodes]
    | otherwise = let
        bigNodes = n `div` 2
        smallNodes = bigNodes - 1
        bigTrees = cbalTree bigNodes
        smallTrees = cbalTree smallNodes

        leftBigTrees = [Branch 'x' t1 t2 | t1 <- bigTrees, t2 <- smallTrees]
        flipTree (Branch 'x' x y) = Branch 'x' y x
        flipTree _ = error "Should not happen"

        in leftBigTrees ++ map flipTree leftBigTrees

-- Problem 56
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ x y) (Branch _ a b) = mirror x b && mirror y a
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric x = mirror x x

-- Problem 57
add :: Ord a => Tree a -> a -> Tree a
add Empty el = leaf el
add tree@(Branch x l r) el = case compare el x of
    EQ -> tree
    LT -> Branch x l' r where l' = add l el
    GT -> Branch x l r' where r' = add r el

construct :: Ord a => [a] -> Tree a
construct = foldl add Empty

-- Problem 58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree


-- Problem 59
hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree el 1 = [leaf el]
hbalTree el n = let
    smallTrees = hbalTree el (n - 2)
    bigTrees = hbalTree el (n - 1)

    smallBig = [Branch el x y | x <- smallTrees, y <- bigTrees]
    bigSmall = [Branch el x y | x <- bigTrees, y <- smallTrees]
    bigBig = [Branch el x y | x <- bigTrees, y <- bigTrees]
    in smallBig ++ bigSmall ++ bigBig

-- Problem 60
minNodes :: Integral a => a -> a
minNodes 1 = 1
minNodes 2 = 2
minNodes n = 1 + 2 * minNodes (n - 1)

maxHeight :: Integral a => a -> a
maxHeight nodes = (nodes + 1) `div` 2