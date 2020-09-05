-- A

-- 4.5
(&&) :: Bool -> Bool -> Bool
(&&) x y =
  if x == True
    then
      if y == True
        then True
        else False
    else False

--4.7
mult :: Integer -> Integer -> Integer -> Integer
mult = \x -> (\y -> (\z -> x * y * z))

-- B

-- 5.6
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], x == sum (factors x) - x]

-- 5.7
listcom :: [(Int, Int)]
listcom = concat [[(x, y) | x <- [1, 2]] | y <- [3, 4]]

-- 5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- C

rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x : xs) y
  | x == y = xs
  | otherwise = x : rem1 xs y

-- D

diff :: Eq a => [a] -> [a] -> [a]
diff [] _ = []
diff (x : xs) [] = x : diff xs []
diff xs (y : ys) = diff (rem1 xs y) ys