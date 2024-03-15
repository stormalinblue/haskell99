module Problems46To50 (
    and',
    or',
    nand',
    nor',
    xor',
    impl',
    equ',
    table,
    table2,
    tablen,
    gray,
    huffman
) where
import Data.List (sortBy)


-- Problem 46, 47
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

infixr 3 `and'`

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

infixr 2 `or'`

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _ _ = True

nor' :: Bool -> Bool -> Bool
nor' False False = True
nor' _ _ = False

xor' :: Bool -> Bool -> Bool
xor' True True = False
xor' False False = False
xor' _ _ = True

impl' :: Bool -> Bool -> Bool
impl' _ True = True
impl' False _ = True
impl' _ _ = False

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False

table :: (Bool -> Bool -> Bool) -> [[Bool]]
table f =
    [
        [True, True, f True True],
        [True, False, f True False],
        [False, True, f False True],
        [False, False, f False False]
    ]

table2 :: (Bool -> Bool -> Bool) -> [[Bool]]
table2 = table

-- Problem 48
tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f =
    let
        truthPoss 0 = [[]]
        truthPoss a = [True:poss | poss <- truthPoss (a - 1)] ++ [False:poss | poss <- truthPoss (a - 1)]

    in [poss ++ [f poss] | poss <- truthPoss n]

-- Problem 49
gray :: Int -> [String]
gray 0 = []
gray 1 = ["0", "1"]
gray n = ['0':code | code <- prevGray] ++ ['1':code | code <- reverse prevGray] where prevGray = gray (n - 1)

-- Problem 50
data HTree a = Leaf {weight :: Int, _payload :: a} | Branch { weight :: Int , _left :: HTree a, _right :: HTree a } deriving Show
huffman :: Show a => [(a, Int)] -> [(a, String)]
huffman [] = []
huffman [(c, _)] = [(c, "0")]
huffman xs = let
    sortByWeight = sortBy (\l r -> compare (weight l) (weight r))

    hufftree :: Show a => [HTree a] -> HTree a 
    hufftree [] = error "This should not happen"
    hufftree [x] = x
    hufftree (x:y:ys) = hufftree $ sortByWeight ((Branch {weight = weight x + weight y, _left = x, _right = y}):ys)
    
    huffcodes :: HTree a -> [(a, String)]
    huffcodes (Branch {_left = l, _right = r}) = [(x, '0':code) | (x, code) <- huffcodes l] ++ [(x, '1':code) | (x, code) <- huffcodes r]
    huffcodes Leaf {_payload = x} = [(x, "")]

    t = hufftree $ sortByWeight $ map (\(x, w) -> Leaf {weight = w, _payload = x}) xs
    in huffcodes t


