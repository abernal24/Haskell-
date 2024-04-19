{-
- You must submit a single .hs file with the
  following name: firstName-lastName-hw2.hs.
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
 
- You will lose 10 points if you include a main function in your file.
-}
 
-- Problem 1: Perfect Numbers
-- Define 'perfects' function as described in the homework
factors :: Int -> [Int]
factors n = [x | x <- [1..n `div` 2], n `mod` x == 0]
perfects :: Int -> [Int]
perfects limit = [x | x <- [1..limit], sum (factors x) == x]
 
-- Problem 2: Scalar Product
-- Define 'scalarproduct' function as described in the homework
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys ]
 
-- Problem 3: Top N
-- Define 'topN' function as described in the homework

topN :: Int -> [(String, Int)] -> [String]
topN n pairs = [name | (name, score) <- pairs, score >= n]

-- Problem 4: Riffle
-- Define 'riffle' function as described in the homework
riffle :: [a] -> [a] -> [a]
riffle [] ys = ys
riffle xs [] = xs
riffle (x:xs) (y:ys) = x : y : riffle xs ys
 
-- Problem 5: Library Functions
-- Define 'and', 'concat', 'replicate', '!!!', and 'elem' functions as described in the homework
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs
 
concat' :: [[a]] -> [a]
concat' [] = []
concat' (ls:xs) = ls ++ concat' xs
 
replicate' :: Int -> a -> [a]
replicate' n x 
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x

 
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x 
(!!!) (x:xs) n = xs !!! (n-1)
 
elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False 
elem' a (x:xs)
  | a == x = True 
  | otherwise = elem' a xs
 
-- Problem 6: Iota Iota
-- Define 'iotaIota' function without using list comprehension
iotaIota :: Int -> [(Int, Int)]
iotaIota n = appendPairs 1 1 n
  where
    appendPairs i j n
      | i > n = []
      | j > n = appendPairs (i + 1) 1 n
      | otherwise = (i,j) : appendPairs i (j+1) n
 
-- Problem 7: Matrix Map
-- Define 'matrixMap' using the 'map' function
matrixMap :: (a -> b) -> [[a]] -> [[b]]
matrixMap f = map (map f)
 
-- Problem 8: Merge
-- Define 'merge' function for merging two sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys                  
merge xs [] = xs 
merge (x:xs) (y:ys)
    | x <= y       = x : merge xs (y:ys)
    | otherwise    = y : merge (x:xs) ys 

-- Problem 9: Merge Sort
-- Define 'msort' using 'merge', and first define 'halve'
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where 
    (left, right) = halve xs
 
halve :: [a] -> ([a], [a])
halve xs = splitAt(length xs `div` 2 + length xs `mod` 2) xs  
 
-- Problem 10: Goldbach
-- Define 'goldbach' that returns all unique prime pairs summing to an even number 

goldbach :: Int -> [(Int, Int)]
goldbach n = [(x, y) | x <- primes n, y <- primes n, x <= y, x + y == n]

prime :: Int -> Bool
prime n = factors n == [1, n]


primes :: Int -> [Int]
primes n = [x | x <-[2..n], prime x]