-- B

-- 1.7 Exercises

-- 4
qsort [] = []
qsort (x : xs) = qsort larger ++ [x] ++ qsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- 5
-- The second element will be dropped
-- because it is not more or less then the first element
-- which is the element it is compared to.
-- To put it another way, it will not satisfy the condition.

-- 2.7 Exercises

-- 4
last (x : xs) = head (reverse xs)

-- 5
rmLast [x] = []
rmLast (x : xs) = x : rmLast xs

--C

-- 1
plu ([], y) = []
plu (x : xs, y) = x + y : plu (xs, y)

-- 2
pali xs = xs == reverse xs