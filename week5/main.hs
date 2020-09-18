import Data.Char

-- 1

data Ast = S String | V Int | P Ast Ast | M Ast Ast deriving (Show)

eval :: Ast -> Int
eval (S x) = error "Cannot do arithmetic operations on strings"
eval (V x) = x
eval (P x y) = eval x + eval y
eval (M x y) = eval x * eval y

-- 2

inn :: Ast -> String
inn (S x) = error "Cannot do arithmetic operations on strings"
inn (V x) = show x
inn (P x y) = "(" ++ inn x ++ " + " ++ inn y ++ ")"
inn (M x y) = "(" ++ inn x ++ " * " ++ inn y ++ ")"

-- 3

-- 3.1
tokens = "*+()"

whitespace = " "

tokenize :: String -> [String]
tokenize [] = []
tokenize (x : xs)
  | elem x t = [x] : tokenize xs
  | elem x s = tokenize xs
  | otherwise = nextToken (t ++ s) (x : xs) : tokenize (skipToken (t ++ s) (x : xs))
  where
    t = tokens
    s = whitespace
    notin ts = (\x -> not (elem x ts))
    nextToken ts xs = takeWhile (notin ts) xs
    skipToken ts xs = dropWhile (notin ts) xs

-- 3.2

parse :: String -> Ast
parse xs = fst $ parseU $ tokenize xs

parseU :: [String] -> (Ast, [String])
parseU [] = error "Too few numbers provided"
parseU ("*" : xs) =
  let (e1, r1) = parseU xs
      (e2, r2) = parseU r1
   in (M e1 e2, r2)
parseU ("+" : xs) =
  let (e1, r1) = parseU xs
      (e2, r2) = parseU r1
   in (P e1 e2, r2)
parseU (x : xs)
  | allDigits x = (V (read x), xs)
  | otherwise = (S x, xs)

allDigits :: [Char] -> Bool
allDigits xs = takeWhile isDigit xs == xs

-- 3.3

ev :: String -> Int
ev s = eval $ parse s

-- 3.4

innfiks :: String -> String
innfiks s = tail $ init $ inn $ parse s