module Essai where

mult x y = x * y


sourceList :: [Int]
sourceList = [1..100]

allTriples :: [(Int, Int, Int)]
allTriples =
  [(a, b, c) | a <- sourceList, b <- sourceList, c <- sourceList]

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c) = a ^ 2 + b ^ 2 == c ^ 2

isSmallEnough :: (Int, Int, Int) -> Bool
isSmallEnough (a, b, c) = a + b + c < 100

finalAnswer :: [(Int, Int, Int)]
finalAnswer = filter 
  (\t -> isPythagorean t && isSmallEnough t)
    allTriples
    
isLeapYear :: Integer -> Bool
isLeapYear year = by4 && ( not by100 || by400)
  where
    by4 = year `mod` 4 == 0
    by100 = year `mod` 100 == 0
    by400 = year `mod` 400 == 0

