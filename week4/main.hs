-- A

-- rekursjon
fjernrek :: String -> Char -> String
fjernrek [] c = []
fjernrek (x : xs) c
  | x == c = fjernrek xs c
  | otherwise = x : fjernrek xs c

-- listekomprehensjon
fjernlist :: String -> Char -> String
fjernlist xs c = [x | x <- xs, x /= c]

-- B

-- rekursjon
tegnposrek :: Char -> String -> [Int]
tegnposrek c xs = tegnposrek' c $ zip xs [0 ..]

tegnposrek' :: Eq t => t -> [(t, a)] -> [a]
tegnposrek' c [] = []
tegnposrek' c (x : xs)
  | c == fst x = snd x : tegnposrek' c xs
  | otherwise = tegnposrek' c xs

-- listekomprehensjon
tegnposlist :: Char -> String -> [Int]
tegnposlist c xs = [y | (x, y) <- zip xs [0 ..], c == x]

-- C

intToList :: Int -> [Int]
intToList x = reverse $ intToList' x

intToList' :: Integral t => t -> [t]
intToList' x
  | x < 10 = [x]
  | otherwise = x `mod` 10 : intToList' (x `div` 10)

-- D

-- a
settSammen :: [String] -> String
settSammen [x] = x
settSammen (x : xs) = x ++ " " ++ settSammen xs

-- b
delStrengen :: String -> [String]
delStrengen [] = []
delStrengen (x : xs)
  | [x] == " " = delStrengen xs
  | otherwise = (fst $ breaking) : delStrengen (snd $ breaking)
  where
    breaking = break (\z -> [z] == " ") (x : xs)

-- c
gdelStrengen :: String -> String -> [String]
gdelStrengen [] s = []
gdelStrengen (x : xs) ss
  | elem x ss = gdelStrengen xs ss
  | otherwise = (fst $ breaking) : gdelStrengen (snd $ breaking) ss
  where
    breaking = break (\z -> elem z ss) (x : xs)