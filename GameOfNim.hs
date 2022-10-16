nim :: IO ()
nim = do
  play [5,4,3,2,1] 0

play :: [Int] -> Int -> IO ()
play board player = do
  displayBoard board
  (l,n) <- getRownAndCount board player
  if sum board - n == 0 then
    do putStr "Player "
       putStr (show (player+1))
       putStrLn " won"
  else
    play (newBoard l n board) (nextPlayer player)

getRownAndCount :: [Int] -> Int -> IO (Int, Int)
getRownAndCount board player = do
  putStrLn $ "Player " ++ show (player+1)
  putStrLn "========"
  x <- getRow board
  y <- getCount board x
  putStr "\n"
  return (x,y)

getRow :: [Int] -> IO Int
getRow board = do
  putStr "Select the desired row: "
  row <- getLine
  let n = read row :: Int in
    if  n > 4 || n < 0 || (board !! n == 0) then
      do
        putStr "Invalide row - "
        getRow board
    else
      return n


getCount :: [Int] -> Int -> IO Int
getCount board row = do
  putStr "How many stars?: "
  count <- getLine
  let n = read count :: Int in
    if n > board !! row then
      do
        putStr "Invalide count - "
        getCount board row
    else
      return n


displayBoard :: [Int] -> IO ()
displayBoard xs = go 0 xs
  where go :: Int -> [Int] -> IO ()
        go _ [] = do putStr "\n"
        go n (x:xs) = do
          putStr $ show n ++ ": "
          putStrLn (replicate x '*')
          go (n+1) xs
  

nextPlayer :: Int -> Int
nextPlayer n = (n+1) `mod` 2
  
newBoard :: Int -> Int -> [Int] -> [Int]
newBoard l n board = replaceElem l new board
  where new = (board !! l) - n


replaceElem :: Int -> a -> [a] -> [a]
replaceElem i elem xs = x1 ++ [elem] ++ tail x2
  where (x1, x2) = splitAt i xs

test = do
  x <- getLine
  let y = read x :: Int in
    if y > 4 then
      do test
    else
      return (x, y)
    

