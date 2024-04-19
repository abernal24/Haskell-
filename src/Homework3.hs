{-
Submission rules:

- All text answers must be given in Haskell comment
  underneath the problem header.

- You must submit a single .hs file with the
  following name: firstName-lastName-hw3.hs.
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

-- Problem 1
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f(x,y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x,y) = f x y


-- Problem 2

unfold:: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [a] -> [[a]]
chop8 = unfold null (take 8) (drop 8) 

map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f


-- Problem 3
concatER :: [[a]] -> [a]
concatER []       = []
concatER (xs:xss) = xs ++ concatER xss

concatFR :: [[a]] -> [a]
concatFR = foldr (++) []

concatFL :: [[a]] -> [a]
concatFL = foldl (++) []


-- Problem 4
disjunction2 :: (a -> Bool) -> (a -> Bool) -> a -> Bool
disjunction2 p1 p2 x = p1 x || p2 x


-- Problem 5
disjunction :: [a -> Bool] -> a -> Bool
disjunction ps x = foldr (\p acc -> p x || acc) False ps


-- Problem 6
deleteDupes':: Eq a => [a] -> [a] 
deleteDupes' []   =  []
deleteDupes' (x:xs) = x : deleteDupes' (filter (/= x) xs)

deleteDupes :: Eq a => [a] -> [a]
deleteDupes = foldr (\x xs -> x : filter (/=x) xs) []


-- Problem 7
tally :: (a -> Bool) -> [a] -> Int
tally p xs = foldl (\acc x -> if p x then acc + 1 else acc) 0 xs


-- Problem 8
bangBang :: [a] -> Int -> a
bangBang xs n = snd $ foldr (\(i, x) acc -> if i == n then (i, x) else acc) (length xs - 1, last xs) (zip [0..] xs)

-- Problem 9
increasing :: Ord a => [a] -> Bool
increasing xs = foldr (\(x,y) r -> if x > y then False else r) True (zip xs (drop 1 xs))


-- Problem 10
decimate :: [a] -> [a]
decimate xs = snd $ foldl (\(i, acc) x -> if mod i 10 == 0 then (i + 1, acc) else (i + 1, acc ++ [x])) (1, []) xs  


-- Problem 11
encipher :: Eq a => [a] -> [b] -> [a] -> [b]
encipher xs ys zs = map (switch(zip xs ys)) zs
  where
    switch ((x,y):xys)z
      |x ==z =y
      |otherwise = switch xys z


-- Problem 12
prefixSum :: Num a => [a] -> [a]
prefixSum xs = prefixSumHelper xs 0 
  where
    prefixSumHelper [] _ = []
    prefixSumHelper (x:xs) acc = newAcc : prefixSumHelper xs newAcc 
      where newAcc = acc + x


-- Problem 13
minesweeper :: [String] -> [String]
minesweeper grid = [ [ cellValue (x, y) | y <- [0 .. length (head grid) - 1] ] | x <- [0 .. length grid - 1] ]
  where
    cellValue (x, y)
      | grid !! x !! y == '*' = '*'
      | otherwise = let adjMines = countAdjacentMines x y
                    in if adjMines == 0 then '.' else intToDigit adjMines
    countAdjacentMines x y = length $ filter (== '*') $ adjacentCells x y
    adjacentCells x y = [ grid !! nx !! ny | nx <- [max 0 (x-1) .. min (length grid - 1) (x+1)],
                                             ny <- [max 0 (y-1) .. min (length (head grid) - 1) (y+1)],
                                             (nx, ny) /= (x, y),
                                             nx >= 0, ny >= 0, nx < length grid, ny < length (head grid) ]


-- Helper function provided 
intToDigit :: Int -> Char
intToDigit 0 = '0'
intToDigit 1 = '1'
intToDigit 2 = '2'
intToDigit 3 = '3'
intToDigit 4 = '4'
intToDigit 5 = '5'
intToDigit 6 = '6'
intToDigit 7 = '7'
intToDigit 8 = '8'
intToDigit 9 = '9'