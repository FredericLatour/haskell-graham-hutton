import Test.HUnit


-- [FP 10 - Chap 7 Higher Order Functions - YouTube](https://www.youtube.com/watch?v=dUPWjM63THs)

-- ## Introduction
-- A function is called higher-order if it takes a function as an argument or 
-- returns a function as a result.
twice :: (a -> a) -> a -> a
twice f x = f (f x)


-- ## Why are they useful?

-- * Common programming idioms can be encoded as functions within the language itself
-- * Domain specific languages (DSL) can be defined as collections of higher-order functions.
-- * Algebraic properties of higher-order functions can be used to reason about programs.


-- ## The Map Function

-- The higher-order library function called `map` applies a function to every element
-- of a list
-- map :: (a -> b) -> [a] -> [b]
-- For example: see tests

-- The map function can be defined in a particularly simple manner using a list comprehension
map' f xs = [f x | x <- xs]

-- alternatively for the purpose of proofs (induction), the map function can also be defined using
-- recursion:
map'' f [] = []
map'' f (x:xs) = f x : map'' f xs


-- ## The Filter Function

-- The higher-order library function filter selects every element from a list that 
-- satisfies a predicate.
-- filter :: (a -> Bool) -> [a] -> [a]
-- For example: see tests at the end


-- The filter function can be defined using a list comprehension
filter' f xs = [x | x <- xs, f x ]

-- alternatively for the purpose of proofs (induction), the filter function can also be defined using
-- recursion:
filter'' f [] = []
filter'' f (x:xs) = if f x then x : filter'' f xs else filter'' f xs

-- alternative
filter''' f [] = []
filter''' f (x:xs) 
  | f x    = x : filter''' f xs
  | otherwise = filter''' f xs



-- ## The Foldr Function
-- A number of functions on lists can be defined using the following simple pattern
-- of recursion :
--
--  f [] = v
--  f (x:xs) = x `fn` f xs
--  f maps the empty list to some value v, and 
--  any non-empty list to some function `fn` applied
--  to its head and f of its tail.

-- For example:

-- sum []      = 0           -- v = 0
-- sum (x:xs)  = x + sum xs  -- fn = +

-- product [] = 1                  -- v = 1
-- product (x:xs) = x * product xs -- fn = *

-- and [] = True             -- v = True
-- and (x:xs) = x && and xs  -- fn = &&


-- The higher-order library function `foldr` (fold right) encapsulates
-- this simple pattern of recursion, with the function `fn` and the
-- value `v` as arguments. For example:
-- sum = foldr (+) 0
-- product = foldr (*) 1
-- and = foldr (&&) True
-- or' = foldr (||) False

-- foldr itself can be defined using recursion
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr f v xs)
-- However, it is best to think of foldr non-recursively as simultaneously
-- replacing each (:) in a list by a given function (fn), and [] by a given value.
-- For example:
-- foldr (+) 0 [1,2,3] => foldr (+) 0 (1:(2:(3:[]))) => 1+2+3+0


-- ## Other Foldr Examples

-- Even though foldr encapsulates a simple pattern of recursion, it can
-- be used to define many more functions than might first be expected.

-- length function
-- ----------------
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + Main.length xs

-- length [1,2,3] => length (1:(2:(3:[]))) => 1+(1+(1+0)))
-- we replace each (:) by \_ n -> 1+n, and [] by 0
-- Hence, we have :
length' :: [a] -> Int
length' = foldr (\_ n -> 1+n) 0

-- reverse function
-- -----------------
-- reverse [] = []
-- reverse (x:xs) = reverse xs ++ [x]

-- reverse [1,2,3] => reverse (1:(2:(3:[])))
-- 
reverse' :: [a] -> [a]
reverse' = Prelude.foldr (\a b -> b ++ [a]) []

-- foldr (\a b -> b ++ [a]) [] [1,2,3] =
--    ( foldr f v [2,3] ++ [1])
--    ( foldr f v [3] ++ [2]) ++ [1]
--    (( foldr f v [] ++ [3]) ++ [2]) ++ [1]
--    ( [] ++ [3] ++ [2] ++ [1])
--     [3,2,1]


-- The append function (++) has a particularly compact definition using foldr:
-- (++ ys) = foldr (:) ys
-- we replace each (:) by (:) and [] by ys.


-- ## Why Is Foldr Useful
--
-- * Some recursive function on lists, such as sum, are simpler to define using foldr
-- * Properties of function defined using foldr can be proved using algebraic properties 
--   of foldr, such as fusion and the banasplit rule.
-- * Advanced program optimisations can be simpler if foldr is used in place of explicit recursion



-- ## Other Library Function
--
-- The library function (.) returns the composition of two functions as a single function
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

-- Example
odd :: Int -> Bool
odd = not Main.. even


-- The libray function `all` decides if every element of a list satisfies a given predicate.
all :: (a -> Bool) -> [a] -> Bool
all p xs = and [p x | x <- xs ]


-- The library function `any` indicates if at least one element of a list satisfies a predicate.
any :: (a -> Bool) -> [a] -> Bool
any p xs = or [p x | x <- xs ]

-- The library function `takeWhile` selects elements from a list while a predicate holds of
-- all the elements.
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs)
  | p x = x : Main.takeWhile p xs
  | otherwise = []

-- The library function `dropWhile` removes elements while a prediate holds of all the elemnts
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs)
  | p x = Main.dropWhile p xs
  | otherwise = x : xs


-- ## Exercises

-- What are higher-order functions that return functions as result better known as ?
-- ==> curried functions
take :: Int -> ([a] -> [a])

-- Express the comprehension [f x | x <- xs, p x] using the function map and filter
ex2 :: (a -> a) -> (a -> Bool) -> [a] -> [a]
ex2 f p xs = [f x | x <- xs, p x]

ex2' :: (a -> a) -> (a -> Bool) -> [a] -> [a]
ex2' f p xs = map f (filter p xs)


-- redefine map f and filter p using foldr
map''' :: (a -> b) -> [a] -> [b]
-- map''' f [1,2,3] => f 1:f 2: f 3:[]
map''' f = foldr (\a b -> f a : b) []

filter'''' :: (a -> Bool) -> [a] -> [a]
-- filter''' p [1,2,3] =>  if p 1 then [1] else [] ++ if p 2 then [2] else [] ++ if p 3 then [3] else [] ++ []
filter'''' p xs = foldr (\a b -> (if (p a) then [a] else []) ++ b) [] xs



-- >runTestTT tests
tests = TestList [
        TestLabel "map" (TestCase (assertEqual "map (+1) [1,3,5,7]" ([2,4,6,8]) (map (+1) [1,3,5,7]))), 
        TestLabel "filter" (TestCase (assertEqual "filter even [1..10]" ([2,4,6,8,10]) (filter even [1..10]))), 
        TestLabel "reverse'" (TestCase (assertEqual "reverse' [1,2,3]" ([3,2,1]) (reverse' [1,2,3])))
        ,TestLabel "length'" (TestCase (assertEqual "length' [1,2,3]" (3) (length' [1,2,3])))
        ,TestLabel "all" (TestCase (assertEqual "all even [2,4,6,8,10]" (True) (Main.all even [2,4,6,8,10])))
        ,TestLabel "any" (TestCase (assertEqual "any (== ' ') 'abc def'" (True) (Main.any (== ' ') "abc def")))
        ,TestLabel "takeWhile" (TestCase (assertEqual "takeWhile (/= ' ') 'abc def'" ("abc") (Main.takeWhile (/= ' ') "abc def")))
        ,TestLabel "dropWhile" (TestCase (assertEqual "dropWhile (== ' ') '   abc'" ("abc") (Main.dropWhile (== ' ') "   abc")))
        ]
