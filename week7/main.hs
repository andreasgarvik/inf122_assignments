-- A

-- 7.1
list :: [a] -> (a -> b) -> (a -> Bool) -> [b]
list xs f p = [f x | x <- xs, p x]

hof :: [a] -> (a -> b) -> (a -> Bool) -> [b]
hof xs f p = (map f . filter p) xs

-- 7.4
dec2int :: [Int] -> Int
dec2int = read . foldl (\xs -> \x -> xs ++ show x) []

-- 7.5
mycurry :: ((a, b) -> c) -> a -> b -> c
mycurry f = \x -> \y -> f (x, y)

myuncurry :: (a -> b -> c) -> (a, b) -> c
myuncurry f = \(x, y) -> f x y

-- 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 = foldl (\xs -> \x -> if even $ length xs then xs ++ [f1 x] else xs ++ [f2 x]) []

-- 8.5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val v) = f v
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 8.6
eval :: Expr -> Int
eval = folde (\x -> x) (\x -> \y -> x + y)

size :: Expr -> Int
size = folde (\_ -> 1) (\x -> \y -> x + y)

-- B

infiks :: Expr -> String
infiks = init . tail . infiks'

infiks' :: Expr -> String
infiks' (Val v) = show v
infiks' (Add x y) = "(" ++ (infiks' x) ++ " + " ++ (infiks' y) ++ ")"

prefiks :: Expr -> String
prefiks = init . prefiks'

prefiks' :: Expr -> String
prefiks' (Val v) = show v ++ " "
prefiks' (Add x y) = "+ " ++ (prefiks' x) ++ (prefiks' y)

postfiks :: Expr -> String
postfiks = tail . postfiks'

postfiks' :: Expr -> String
postfiks' (Val v) = " " ++ show v
postfiks' (Add x y) = (postfiks' x) ++ (postfiks' y) ++ " +"