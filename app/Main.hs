module Main where
import Test.HUnit

main :: IO ()
main = putStrLn (show $ [("mult 5 5", mult 5 5), ("add 2 3", add 2 3), ("Sum' [2,4,6,8]", sum' [2,4,6,8])])
-- add "t" 5


-- adding 2 ints
add :: Int -> Int -> Int
add x y = x + y

mult x y = x * y

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

testadd = TestCase (assertEqual "add 2 2" (4) (add 2 2))
testmult = TestCase (assertEqual "mult 2 2" (3) (mult 2 2))
tests = TestList [TestLabel "add" testadd, TestLabel "mult" testmult]
