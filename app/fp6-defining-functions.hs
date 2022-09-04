abs' n 
  | n < 0 = -n
  | otherwise = n

signum' n | n < 0     = -1
          | n == 0    = 0
          | otherwise = 1

addLambda :: Int -> (Int -> Int)
addLambda = \x -> (\y -> x + y)

odds n = map f [0..n-1]
  where f x = x*2 +1

odds' n = map (\x -> x*2 + 1) [0..n-1]



-- Exercice 1
-- Consider a function `safetail` that behaves in the same way as tail, 
-- except that safetail maps the empty list to the empty list, whereas
-- tail gives an error in this case. 
-- Sefine `safetail` using:
--  a. a contidional expression
--  b. guarded equations
--  c. pattern matching

-- Hint: the library function null :: [a] -> bool can be used 
-- to test if a list is empty

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail'' :: [a] -> [a]
safetail'' xs
  | null xs   = []
  | otherwise = tail xs

safetail' :: [a] -> [a]
safetail' [] = []
safetail' (x:xs) = xs

-- Exercice 2
-- Give three possible definitions for the logical or operator (||)
-- using pattern matchtin
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

False || False = False
_ || _ = True

False || b = b
True || _ = True


-- Exercice 3
-- Redefine the following version (&&) using conditionals rather than patterns
--    True && True = True
--    _ && _ = False

(&&) :: Bool -> Bool -> Bool
(&&) x y = if a == True && b == True then True else False


-- Exercice 4
-- Do the same for the following version:
--    True && b = b
--    False && _ = False
(&&) :: Bool -> Bool -> Bool
(&&) x y = if a == True then b else False



