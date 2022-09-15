module Essai where


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



-- data Planet = Mercury
--             | Venus
--             | Earth
--             | Mars
--             | Jupiter
--             | Saturn
--             | Uranus
--             | Neptune
--             deriving (Eq)

-- data Orbital Planet Float = Planet (0.2408467)

-- ageOn :: Planet -> Float -> Float
-- -- ageOn Mercury seconds = ageOn Earth seconds / (0.2408467) 
-- -- ageOn Venus seconds = ageOn Earth seconds / (0.61519726)
-- -- ageOn Earth seconds = seconds / 31557600
-- -- ageOn Mars seconds = ageOn Earth seconds / (1.8808158)
-- -- ageOn Jupiter seconds = ageOn Earth seconds / (11.862615)
-- -- ageOn Saturn seconds = ageOn Earth seconds / (29.447498)
-- -- ageOn Uranus seconds = ageOn Earth seconds / (84.016846)
-- -- ageOn Neptune seconds = ageOn Earth seconds / (164.79132)

-- ageOn planet seconds
--   | planet == Mercury = ageOn Earth seconds / (0.2408467) 
--   | planet == Venus = ageOn Earth seconds / (0.61519726)
--   | planet == Mars = ageOn Earth seconds / (1.8808158)
--   | planet == Jupiter = ageOn Earth seconds / (11.862615)
--   | planet == Saturn = ageOn Earth seconds / (29.447498)
--   | planet == Uranus = ageOn Earth seconds / (84.016846)
--   | planet == Neptune = ageOn Earth seconds / (164.79132)
--   | otherwise = seconds / 31557600

data Planet = Planet Float
mercury, earth, venus :: Planet
mercury = Planet (0.2408467)
venus = Planet (0.61519726)
earth = Planet (1.0)

