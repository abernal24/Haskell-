{-
Submission rules:

- All text answers must be given in Haskell comment
  underneath the problem header.

- You must submit a single .hs file with the
  following name: firstName-lastName-hw4.hs.
  Failure to do so will result in -10 points.
  
- You will lose 10 points if you put a module statement
  at the top of the file.

- You will lose 10 points for any import statements you have
  in your file and will automatically miss any problems you used
  an imported function on.
  
- If your file doesn't compile you will lose 10 points and miss any
  problems that were causing the compilation errors.

- This means that any function which is causing compiler errors should
  be commented out. There will be no partial credit.
  
- You must use the skeleton file provided and must not alter any type
  signature. If you alter a type signature you will automatically miss
  that problem.

- You will lose 10 points if you include a *main* function in your file.
-}

-- Problem 1 (Exercise 8.1)

data Nat = Zero | Succ Nat
    deriving (Eq, Show)

nat2Int :: Nat -> Int
nat2Int Zero     = 0
nat2Int (Succ n) = 1 + nat2Int n

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n2      = n2 
add (Succ n1) n2 = Succ (add n1 n2)

mult :: Nat -> Nat -> Nat
mult Zero _       = Zero
mult _ Zero       = Zero
mult n1 (Succ n2) = add n1 (mult n1 n2)


-- Tests

-- >>> mult Zero (Succ Zero)
-- Zero

-- >>> mult (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))
-- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

-- >>> mult (Succ (Succ (Succ Zero))) (Succ (Succ (Succ Zero)))
-- Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))

-- Problem 2 (Exercise 8.3)

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Eq, Show)

-- Function to count 
countLeaves :: Tree a -> Int 
countLeaves (Leaf _) = 1 
countLeaves (Node left right) = countLeaves left + countLeaves right 

balanced :: Tree a -> Bool
balanced (Leaf _) = True 
balanced (Node left right) = 
  let leftCount  = countLeaves left 
      rightCount = countLeaves right 
  in abs (leftCount - rightCount) <= 1 && balanced left && balanced right

-- Tests    

-- >>> balanced (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5))))
-- True

-- >>> balanced (Node (Node (Node (Leaf 1) (Leaf 1)) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5))))
-- True

-- >>> balanced (Node (Node (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5))))
-- False

-- Problem 3 (Exercise 8.4) 
splitList :: [a] -> ([a], [a])
splitList xs = let n = length xs `div` 2 in (take n xs, drop n xs)

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs  = let (left, right) = splitList xs
              in Node (balance left) (balance right)


-- Tests

-- >>> balance [1,2,3,4,5]
-- Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5)))

-- Problem 4 (Exercise 8.5)

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n) = f n 
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- Tests

-- >>> folde (+1) (*) (Add (Add (Val 1) (Val 2)) (Val 3))
-- 24

-- >>> folde (\i -> [i]) (++) (Add (Add (Val 1) (Val 2)) (Val 3))
-- [1,2,3]

-- Problem 5 (Exercise 8.6)


eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-- Tests

-- >>> eval (Add (Add (Val 1) (Val 2)) (Val 3))
-- 6

-- >>> size (Add (Add (Val 1) (Val 2)) (Val 3))
-- 3

-- Tests
-- >>> eval (Add (Add (Val 1) (Val 2)) (Val 3))
-- 6

-- >>> size (Add (Add (Val 1) (Val 2)) (Val 3))
-- 3

-- Problem 6

data ComplexInteger = ComplexInteger Int Int 

real :: ComplexInteger -> Int
real (ComplexInteger x _) = x

imaginary :: ComplexInteger -> Int
imaginary (ComplexInteger _ y) = y

instance Eq ComplexInteger where
   (ComplexInteger x y) == (ComplexInteger u v) = (x == u) && (y == v)

instance Show ComplexInteger where
  show (ComplexInteger x y) = show x ++ "+" ++ show y ++ "i"

instance Num ComplexInteger where
  -- Addition of two complex integers
  (ComplexInteger x y) + (ComplexInteger u v) = ComplexInteger (x + u) (y + v)
  -- Multiplication of two complex integers
  (ComplexInteger x y) * (ComplexInteger u v) = ComplexInteger (x * u - y * v) (x * v + y * u)

-- Tests

-- >>> real (ComplexInteger 1 2)
-- 1

-- >>> imaginary (ComplexInteger 1 2)
-- 2

-- >>> (ComplexInteger 1 2) == (ComplexInteger 3 4)
-- False

-- >>> ComplexInteger 1 2
-- 1+2i

-- >>> (ComplexInteger 1 2) * (ComplexInteger 3 4)
-- -5+10i


-- Problem 7

chopN :: Int -> [a] -> [[a]]
chopN n xs 
  | length xs < n = []
  | otherwise = take n xs : chopN n (drop n xs)

-- Tests

-- >>> chopN 4 [1..10]
-- [[1,2,3,4],[5,6,7,8]]

-- >>> chopN 8 [1..10]
-- [[1,2,3,4,5,6,7,8]]

-- >>> chopN 1 [1..10]
-- [[1],[2],[3],[4],[5],[6],[7],[8],[9],[10]]

-- >>> chopN 2 [1..10]
-- [[1,2],[3,4],[5,6],[7,8],[9,10]]

-- Problem 8  

subAlphabet :: (Eq a, Enum a) => a -> a -> [a] -> [a]
subAlphabet start end initials = uniqueInitials ++ rest
  where
    fullRange = enumFromTo start end
    uniqueInitials = unique initials
    rest = filter (`notElem` uniqueInitials) fullRange

    -- Helper function to remove duplicates while preserving order
    unique :: Eq a => [a] -> [a]
    unique [] = []
    unique (x:xs) = x : unique (filter (/= x) xs)


-- Tests

-- >>> subAlphabet 'A' 'Z' "ZEBRAS"
-- "ZEBRASCDFGHIJKLMNOPQTUVWXY"

-- >>> subAlphabet 1 26 [1,4,6,2,9,10,23,17]
-- [1,4,6,2,9,10,23,17,3,5,7,8,11,12,13,14,15,16,18,19,20,21,22,24,25,26]

-- Problem 9

data Polynomial = Constant Int | MoreTerms Int Polynomial

p = MoreTerms 3 (MoreTerms 4 (Constant 5))

-- instance Show 
instance Show Polynomial where
    show = showPoly 0
      where
        showPoly :: Int -> Polynomial -> String
        showPoly n (Constant c) = show c ++ (if n > 0 then "x" ++ (if n > 1 then "^" ++ show n else "") else "")
        showPoly n (MoreTerms c p) =
          let currentTerm = show c ++ (if n > 0 then "x" ++ (if n > 1 then "^" ++ show n else "") else "")
              nextTerm = showPoly (n + 1) p
          in if null nextTerm then currentTerm
             else if null currentTerm then nextTerm
             else currentTerm ++ " + " ++ nextTerm

   
addPoly :: Polynomial -> Polynomial -> Polynomial
addPoly (Constant c) (Constant d) = Constant (c + d)
addPoly (Constant c) (MoreTerms d p) = MoreTerms (c + d) p
addPoly (MoreTerms c p) (Constant d) = MoreTerms (c + d) p
addPoly (MoreTerms c p) (MoreTerms d q) = MoreTerms (c + d) (addPoly p q)

multPoly :: Polynomial -> Polynomial -> Polynomial
multPoly (Constant c) (Constant d) = Constant (c * d)
multPoly (Constant c) (MoreTerms d p) = MoreTerms (c * d) (multPoly (Constant c) p)
multPoly (MoreTerms c p) (Constant d) = MoreTerms (c * d) (multPoly p (Constant d))
multPoly (MoreTerms c p) (MoreTerms d q) = addPoly (MoreTerms (c * d) (multPoly (Constant c) q)) (multPoly p (MoreTerms d q))


evalPoly :: Polynomial -> Int -> Int
evalPoly (Constant c) _ = c
evalPoly (MoreTerms c p) x = c + x * evalPoly p x


-- Tests

-- >>> p
-- 3 + 4x + 5x^2

-- >>> evalPoly p 2
-- 33

-- Problem 10

data Pair a b = Pair a b

-- Instance of Eq for Pair
instance (Eq a, Eq b) => Eq (Pair a b) where
    Pair x1 y1 == Pair x2 y2 = x1 == x2 && y1 == y2

-- Instance of Ord for Pair
instance (Ord a, Ord b) => Ord (Pair a b) where
    compare (Pair x1 y1) (Pair x2 y2)
        | x1 == x2  = compare y1 y2
        | otherwise = compare x1 x2

-- instance (Eq a, Eq b) => Eq (Pair a b) where

-- instance (Ord a, Ord b) => Ord (Pair a b) where

-- Tests

-- >>> Pair 1 2 == Pair 1 2
-- True

-- >>> Pair 1 2 == Pair 2 1
-- False


-- >>> Pair 1 2 < Pair 2 1
-- True

-- >>> Pair 1 2 > Pair 1 1
-- True

-- >>> Pair 1 2 > Pair 1 3
-- False


-- Problem 11

safeDivide :: Float -> Float -> Maybe Float
safeDivide x y = if y == 0 then Nothing else Just (x / y)

safeDivide' :: Maybe Float -> Maybe Float -> Maybe Float
safeDivide' (Just x) (Just y) = if y == 0 then Nothing else Just (x / y)
safeDivide' _ _ = Nothing

hm xs = 
  if any (== 0) xs 
  then Nothing
  else let inverted = map (1/) xs
           sumInv = sum inverted
           n = fromIntegral $ length xs
       in safeDivide n sumInv

-- Tests

-- >>> hm [2.0, 2.0]
-- Just 2.0

-- >>> hm [1.0, 1.0]
-- Just 1.0

-- >>> hm [0.5, 0.5, 1.0]
-- Just 0.6

-- >>> hm [1.0, -1.0]
-- Nothing

-- >>> hm [1.0, -2.0, -2.0]
-- Nothing

-- >>> hm [1.0, -2.0]
-- Just 4.0