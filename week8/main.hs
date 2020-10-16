-- 1

-- a
alrec :: [Bool] -> Bool
alrec [] = True
alrec (x : xs) = x && alrec xs

-- b
allib :: [Bool] -> Bool
allib xs = filter (\x -> not x) xs == []

-- c
alfdl :: [Bool] -> Bool
alfdl = foldl (\b -> \x -> x && b) True

-- d
alfdr :: [Bool] -> Bool
alfdr = foldr (&&) True

-- 2

ala :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
ala b x = foldr b x

-- 3

trekant :: Int -> IO ()
trekant h = putStr (draw 1 h)

draw :: (Eq t, Num t) => t -> t -> String
draw l h
  | l == h = char "* " l ++ "\n"
  | otherwise = char "* " l ++ "\n" ++ draw (l + 1) h

char :: (Eq t, Num t) => [a] -> t -> [a]
char _ 0 = []
char s i = s ++ char s (i -1)

-- 4

juletre :: Int -> IO ()
juletre h = putStr (drawtre 1 h)

drawtre :: (Eq t, Num t) => t -> t -> String
drawtre l h
  | l == h = char " " (h - l) ++ char " *" l ++ "\n"
  | otherwise = char " " (h - l) ++ char " *" l ++ "\n" ++ drawtre (l + 1) h