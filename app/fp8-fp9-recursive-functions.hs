-- [FP 8 - Recursive Functions - YouTube](https://www.youtube.com/watch?v=WawJ8LArl54)


-- ## Introduction
-- Many functions can naturally be defined in terms of other functions.

fac :: Int -> Int
fac n = product [1..n]

{-- 
Expressions are evaluated by a stepwise process of applying functions to 
their arguments.

For example: 

  fac 4
=
  product [1..4]
=
  product [1,2,3,4]
=
  1*2*3*4
=
  24

--}

{-- 
## Recursive Functions

In Haskell, functions can also be defined in terms of themselves. Such functions are called recursive.
--}
fac' :: Int -> Int
fac' n = n * fac' (n-1)

{-- 
Example:

    fac 3
  =
    3 * fac 2
  =
    3 * (2 * fac 1)
  =
    3 * (2 * (1 * fac 0))
  =
    3 * (2 * (1 * 1))
  =
    3 * (2 * 1)
  =
    3 * 2
  =
    6

Note:

* fac 0 = 1 is appropriate because 1 is the identity for multiplication: 1*x = x = x*1
* The recursive definition diverges on integers < 0 because the base case is never reached:
  fac (-1) -- Exception stack overflow
--}

{-- 
## Why is Recursion Useful ?

* Some functions, such as factorial, are simpler to define in terms of other functions.
* However, many functions can naturally be defined in terms of themselves.
* Properties of functions defined using recursion can be proved using mathematical technique of induction.
--}


{-- 
## Recursion on Lists
--}
product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product ns

{-- 
Example:

  product [2,3,4]
=
  2 * product [3,4]
=
  2 * (3 * product [4])
=
  2 * (3 * (4 * product []))
=
  2 * (3 * (4 * 1))
=
  24
--}

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' (xs)

{-- 
Example:

  length [2,3,4]
=
  1 + length [3,4]
=
  1 + (1 + length [4])
=
  1 + (1 + (1 + length []))
=
  1 + (1 + (1 + 0))
= 3
--}
  
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

{-- 
Example:

  reverse' [2,3,4]
=
  reverse' [3,4] ++ [1]
=
  (reverse' [4] ++ [3]) ++ [1]
=
  ((reverse' [] ++ [4]) ++ [3]) ++ [1]
=
= [4,3,2]
--}

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


-- Remove the first n elements from a list
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) (xs)

-- Appending two lists
addList :: [a] -> [a] -> [a]
addList [] xs = xs
addList (x:xs) ys = x : (addList xs ys)

{-- 
Example:

  (++) [1,2] [3,4]
=
  1 : ((++) [2] [3,4] )
=
  1 : ( 2 : ( ++ [] [3,4]) )
=
  1 : ( 2 : ( [3,4] )
=
= [1,2,3,4]
--}


-- Quicksort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where 
      smaller = [a | a <- xs, a < x]
      larger  = [a | a <- xs, a > x]



-- Exercice 1
-- **********

-- Without looking at the standard prelude, define the following library functions 
-- using recursion:

-- * Decide if all logical values in a list are true [x]:
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && Main.and xs

-- * Concatenate a list of lists [x]:
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ Main.concat xss

-- * Produce a list with n identical elements:
replicate :: Int -> a -> [a]
replicate 0 a = []
replicate n a = [a] ++ Main.replicate (n-1) a

--    better solution
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n-1) x


-- * Select the nth element of a list
(!!) :: [a] -> Int -> a
-- xs !! n, n > length xs = error
[] !! _ = error "empty list"
(x:xs) !! 0  = x
(x:xs) !! n = if n > length xs then error "Index out of range" else xs Main.!! (n-1)


-- * Decide if a value is an element of a list
elem :: Eq a => a -> [a] -> Bool
elem a [] = False
elem a (x:xs) = (a == x) || Main.elem a xs

{-- 
## Exercice 2
Define a recursive function
  merge :: Ord a => [a] -> [a] -> [a]

that merges two sorted lists of values to give a sinle sorted list. For Example :

merge [2,5,6] [1,3,4] -- [1,2,3,4,5,6]
--}

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys


{-- 
## Exercice 3
Define a recursive function
  msort :: Ord a => [a] -> [a]

that implements merge sort which can be specified by the following two rules:
  * lists of length <= 1 are already sorted
  * Other lists can be sorted by sorting the two halves and merging the resulting lists.

--}

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where (ys, zs) = halve xs


halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
        where n = length xs `div` 2

-- Insert a Integer into a sorted list
-- insert 3 [1,2,4,5] -- [1,2,3,4,5]
insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs) = if n > x then x : insert n xs else n : (x:xs)


-- Insertion Sort
--  * An empty list is already sorted
--  * insert the first element
isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)