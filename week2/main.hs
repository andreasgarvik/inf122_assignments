-- B

-- 3.3
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- C

-- False :: Bool
-- 5 + 8 :: Integer
-- (+) 2 :: Integer -> Integer
-- (+2) :: Integer -> Integer
-- (2+) :: Integer -> Integer
-- (["foo", "bar"], 'a') :: ([String], Char)
-- [(True, []), (False, [['a']])] :: [(Bool, [[Char]])]
-- \x y -> y !! x :: Int -> [a] -> a
--[ take, drop, \x y -> ( y !! x ) ] :: Feil
--[ take, drop, \x y -> [ y !! x ] ] :: [Int -> [a] -> [a]]

-- D
foo1 :: a -> b -> (a, b)
foo1 x y = (x, y)

foo2 :: a -> b -> (a, b)
foo2 x = \y -> (x, y)

foo3 :: a -> b -> (a, b)
foo3 = \x y -> (x, y)

foo4 :: a -> b -> (a, b)
foo4 = \x -> \y -> (x, y)

foo5 :: a -> b -> (b, a)
foo5 = \x -> \y -> (y, x)

foo6 :: a -> b -> (a, b)
foo6 = \y -> \x -> (y, x)

-- foo[1..4] og foo6 er like, mens foo5 er annerledes

-- E

f1 :: a -> (a, a)
f1 x = (x, x)

f2 :: (a, b) -> a
f2 (x, y) = x

f3 :: (a, b) -> b
f3 (x, y) = y

f4 :: a -> b -> a
f4 x y = x

f5 :: a -> b -> b
f5 x y = y

-- F

f :: Num a => a -> a -> a
f x y = x + y

g :: Num a => (a, a) -> a
g (x, y) = x + y
