-- [FP 7 - List Comprehensions - YouTube](https://www.youtube.com/watch?v=xy5rciaS2ys)

-- The expression x <- [1..5] is called a generator as it states how to
-- generate values for x.

-- Comprehensions can have multiple generators separated by commas. Example :
multGen = [(x,y) | x <- [1,2,3], y <- [4,5]] -- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- Changing the order of the generators change the order of the elements
-- in the final list :
multGen' = [(x,y) | y <- [4,5], x <- [1,2,3]] -- [(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]

-- Multiple generators are like nested loops, with later generators as more deeply
-- nested llops whose variables change more frequently

-- Later generators can depend on the variables that are introduced
-- by earlier generators
dependantGen = [(x,y) | x <- [1..3], y <- [x..3]] -- [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]


concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]


-- List comprehensions can use guards to restric the values produced by earlier
-- generators
compGuard = [x | x <- [1..10], even x] -- [2,4,6,8,10]


factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0 ]

-- A positive integer is prime if its only factors are 1 and itself
prime :: Int -> Bool
prime n = factors n == [1,n]


-- List of all primes up to a given limit
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]


allprimes :: [Int]
allprimes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]


-- The Zip Function
-- ****************

-- zip maps 2 lists to a list of pairs of their corresponding elements.
-- using zip, we can define a function that returns the list of all pairs of 
-- adjacent elements from a list
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- using pairs we can define a function that decides if the elements in a list 
-- are sorted :
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

-- Using zip we can define a function that return the list of all positions of
-- a value in a list :
positions :: Eq a => a -> [a] -> [Int]
positions n xs = [snd x | x <- zip xs [0..length xs ], fst x == n ]
positions' n xs = [y | (x, y) <- zip xs [0..length xs ], x == n]


-- String Comprehensions
-- *********************

-- A string is a sequence of characters enclosed in double quotes.
-- Internally however, strings are represented as lists of characters.
-- "abc" :: String means ['a', 'b', 'c'] :: [char]

-- Because strings are just special kinds of lists, any polymorphic function
-- that operates on lists can also be applied to string. for example :
e1 = length "abcde" -- 5
e2 = take 3 "abcde" -- "abc"
e3 = zip "abc" [1,2,3,4] -- [('a',1), ('b',2), ('c',3)]

-- List comprehensions can also be used to define functions on strings :
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x' == x]



-- Exercice 1
-- **********
-- A triple (x,y,z) of positive integers is called pythagorean if x²+y²=z².
-- Using a list comprehension, define a function
--    pyths :: Int -> [(Int, Int, Int)]
-- that maps an integer n to all such triples with components [1..n]. For example :
-- pyths 5 -- [(3,4,5), (4,3,5)]
pyths :: Int -> [(Int, Int, Int)]
pyths z = [(x,y,z) | x <- [1..z], y <- [1..z], x^2 + y^2 == z^2]



-- Exercice 2
-- **********
-- A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.
-- Using a list comprehension define a function 
--      perfects :: Int -> [Int]
-- that returns the list of all perfect numbers up to a given limit. For example :
-- perfects 500 -- [6, 28, 496]
perfects :: Int -> [Int]
perfects lim = [x | x <- [1..lim], (sum $ init $ factors x) == x]


-- Exercice 3
-- **********
-- The scalar product of two lists of integers xs and ys of length n is given by the sum of the 
-- products of the corresponding integers
--  n-1
-- Sigma (xsi * ysi)
--  i = 0
-- Using a list comprehension, define a function taht returns the scalar product of 2 lists.

scalarProd :: [Int] -> [Int] -> Int
scalarProd xs ys = sum [x * y | (x, y) <- zip xs ys]
scalarProd' xs ys = sum [xs!!i * ys!!i | i <- [0..n-1] ]
                      where n = length xs
