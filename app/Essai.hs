module Essai where
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)

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

isPangram :: String -> Bool
isPangram text = and $ map (\e -> elem e lowerText) ['a'..'z']
                  where lowerText = map toLower text


responseFor :: String -> String
responseFor xs 
  | null resp = "Fine. Be that way!"
  | upperCase resp && question resp = "Calm down, I know what I'm doing!" 
  | question resp = "Sure."
  | upperCase resp = "Whoa, chill out!"
  | otherwise = "Whatever."
  where resp = filter (not.isSpace) xs
        question resp = last resp == '?' :: Bool
        upperCase resp = let alphaChars = filter isAlpha resp
                        in (not $ null alphaChars) && (all isUpper alphaChars)

testUp xs 
  | null mxs = "Null"
  | otherwise = "Not Null"
  where mxs = filter (not . isSpace) xs

upperCase resp = let alphaChars = filter isAlpha resp
                 in (not $ null alphaChars) && (all isUpper alphaChars)

upperCase' xs = all isUpper resp
                where resp = filter (not.isSpace) xs

responseFor' :: Text -> Text
responseFor' t 
  | T.null resp = T.pack "Fine. Be that way!"
  | upperCase resp && question resp = T.pack "Calm down, I know what I'm doing!" 
  | question resp = T.pack "Sure."
  | upperCase resp = T.pack "Whoa, chill out!"
  | otherwise = T.pack "Whatever."
  where resp = T.strip t
        question resp = T.last resp == '?'
        upperCase resp = let alphaChars = T.filter isAlpha resp
                        in (not $ T.null alphaChars) && (alphaChars == T.toUpper alphaChars)



someText = T.pack "   Ceci, est un test ?   "
