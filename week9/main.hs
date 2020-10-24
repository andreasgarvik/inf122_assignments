import Data.Char (isDigit)

-- A
brett :: Int -> IO ()
brett rows = do
  clr
  writeTop rows
  writeRows 1 rows
  putStr "\n"

clr :: IO ()
clr = putStr "\ESC[2J"

clrLine :: (Int, Int) -> IO ()
clrLine (x, y) = do
  goto (x, y)
  putStr "\ESC[2K"

clrMsg :: Int -> IO ()
clrMsg rows = do
  clrLine (rows + 3, 0)
  clrLine (rows + 4, 0)

writeTop :: Int -> IO ()
writeTop rows = writeAt (0, 4) (numbers rows)

writeRows :: Int -> Int -> IO ()
writeRows row tot
  | row == tot + 1 = return ()
  | otherwise = do
    writeRow row tot
    writeRows (row + 1) tot

writeRow :: Int -> Int -> IO ()
writeRow row tot = writeAt (1 + row, if row > 9 then 1 else 2) (numbersAndDots row tot)

numbers :: Int -> String
numbers rows = concat [show (mod n 10) ++ " " | n <- [1 .. rows]] ++ "\n"

numbersAndDots :: Int -> Int -> String
numbersAndDots row n = show row ++ concat (take n (repeat " ."))

writeAt :: (Int, Int) -> String -> IO ()
writeAt (x, y) xs = do
  goto (x, y)
  putStr xs

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show x ++ ";" ++ show y ++ "H")

-- B

play :: Int -> IO ()
play rows = do
  brett rows
  putStrLn
    "To draw an X, write \"n x y\", to erase an X write \"d x y\""
  clrMsg rows
  loop rows

loop :: Int -> IO ()
loop rows = do
  line <- getCmd
  let cmd = words line
  readCmd cmd rows

readCmd :: [String] -> Int -> IO ()
readCmd cmd rows
  | head cmd == "q" = return ()
  | head cmd == "n" = do
    chechPos cmd rows
    writeAt (pos cmd) "X"
    clrMsg rows
    loop rows
  | head cmd == "d" = do
    chechPos cmd rows
    writeAt (pos cmd) "."
    clrMsg rows
    loop rows
  | otherwise = do
    writeError "Unknown command" rows
    loop rows

writeError :: String -> Int -> IO ()
writeError err rows = do
  clrLine (rows + 3, 0)
  writeAt (rows + 3, 0) err
  clrLine (rows + 4, 0)

getCmd :: IO String
getCmd = do
  line <- getLine
  if length line /= 0 then return line else getCmd

chechPos :: [String] -> Int -> IO ()
chechPos cmd rows =
  if length cmd < 3 || not (allDigits (tail cmd))
    then do
      writeError "Provide a \"n\" or \"d\" and two numbers" rows
      loop rows
    else
      if x > rows || y > rows || x < 1 || y < 1
        then do
          writeError ("Provide a position within the board 1-" ++ show rows) rows
          loop rows
        else return ()
  where
    x = read (cmd !! 2)
    y = read (cmd !! 1)

pos :: [String] -> (Int, Int)
pos cmd = (x + 1, y + ((y - 1)) + 3)
  where
    x = read (cmd !! 2)
    y = read (cmd !! 1)

allDigits :: [String] -> Bool
allDigits [] = True
allDigits (x : xs) = allDigits xs && takeWhile isDigit x == x
