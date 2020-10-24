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

clrMsg :: IO ()
clrMsg = do
  clrLine (6, 0)
  clrLine (7, 0)

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
play :: IO ()
play = do
  brett 3
  putStrLn
    "To draw an X, write \"n x y\", to erase an X write \"d x y\""
  clrMsg
  loop

loop :: IO ()
loop = do
  line <- getCmd
  let cmd = words line
  readCmd cmd

readCmd :: [String] -> IO ()
readCmd cmd
  | head cmd == "q" = return ()
  | head cmd == "n" = do
    chechPos cmd
    writeAt (pos cmd) "X"
    clrMsg
    loop
  | head cmd == "d" = do
    chechPos cmd
    writeAt (pos cmd) "."
    clrMsg
    loop
  | otherwise = do
    writeError "Unknown command"
    loop

writeError :: String -> IO ()
writeError err = do
  clrLine (6, 0)
  writeAt (6, 0) err
  clrLine (7, 0)

getCmd :: IO String
getCmd = do
  line <- getLine
  if length line /= 0 then return line else getCmd

chechPos :: [String] -> IO ()
chechPos cmd =
  if length cmd < 3 || not (allDigits (tail cmd))
    then do
      writeError "Provide a \"n\" or \"d\" and two numbers"
      loop
    else
      if x > 3 || y > 3 || x < 1 || y < 1
        then do
          writeError "Provide a position within the board 1-3"
          loop
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
