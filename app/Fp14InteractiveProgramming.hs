module Fp14InteractiveProgramming where
import Data.List
import System.IO


act :: IO (Char, Char)
act = do
  x <- getChar
  getChar
  y <- getChar
  return (x,y)


getLine' :: IO String
getLine' = do 
    x <- getChar
    if x == '\n' then
      return []
    else
      do xs <- getLine'
         return (x:xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr xs
                  putChar '\n'

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine'
            putStr "The string has "
            putStr (show (length xs))
            putStrLn' " characters"



-- Hangman game
hangman :: IO ()
hangman = do  putStrLn "Think of a word: "
              word <- sgetLine
              putStrLn "Try to guess it:"
              play word
      

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do  putChar x
                    return []
              else
                do  putChar '-'
                    xs <- sgetLine
                    return (x:xs)


getCh :: IO Char
getCh = do  hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x

play :: String -> IO ()
play word = do
  putStr "? "
  guess <- getLine
  if guess == word then 
    putStrLn "You got it"
  else
    do  putStrLn (match word guess)
        play word

match :: String -> String -> String
match word guess = [if elem x guess then x else '-' | x <- word]


