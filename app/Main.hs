module Main where
import Essai

main :: IO ()
main = putStrLn (show $ [("mult 5 5", mult 5 5), ("add 2 3", add 2 3), ("Sum' [2,4,6,8]", sum' [2,4,6,8])])
-- add "t" 5


-- adding 2 ints
add :: Int -> Int -> Int
add x y = x + y

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs
