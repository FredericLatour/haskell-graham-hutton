module Fp13TheCountdownProblem where
import Test.HUnit
import Data.List

-- [FP 13 - Chapter 9 - The Countdown Problem - YouTube](https://www.youtube.com/watch?v=CiXDS3bBBUo)


-- ## What is Countdown ?

-- * A popular quiz programme on British television that has been running since 1983.
-- * Base upon an original French version called "Des Chiffres et Des Lettres"
-- * Includes a numbers game that we shall refer to as the countdown problem

-- ## Example

-- Using the numbers:
--    1 3 7 10 25 50
-- and the arithmetic operators:
--    + - * /
-- construct an expression whose value is 765

-- My own take
-- (50+1) * (25-10)
-- (50+25+10) * (7+3-1)




-- ## Rules

-- * All the numbers, including intermediate results, must be positive naturals (1,2,3,...)
-- * Each of the source numbers can be used at most once when constructing the expession.
-- * We abstract from other rules that are adopted on television for pragmatic reasons (limited time, etc...)


-- ## Solutions

-- * There are 780 solutions for this example.
-- * Changing the target number to 831 gives an example that has no solutions.



-- ## Evaluating Expressions

-- Operators:
data Op = Add | Sub | Mul | Div -- deriving(Show)
instance Show Op where 
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"


-- Apply an operator
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


-- Decide if the result of applying an operator to two positive natural numbers is another such:
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- Expressions:
data Expr = Val Int | App Op Expr Expr
instance Show Expr where 
  show (Val n) = show n
  show (App op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"

-- Evaluation:
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]
-- Either succeeds and returns a singleton list, or fails and returns the empty list


-- (50+1) * (25-10)
someExpr :: Expr
someExpr = App Mul (App Add (Val 50) (Val 1)) (App Sub (Val 25) (Val 10))

-- ## Formalising The Problem

-- Return a list of all possible ways of choosing zero o more elements from a list
choices :: [a] -> [[a]]
-- for example: choises [1,2] => [[], [1], [2], [1,2], [2,1]]
choices xs = concat [ permutations s | s <- subsequences xs ]


-- Return a list of all the values in an expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- Decides if an expression is a solution for a given list of source
-- numbers and a target number:
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]



-- (50+25+10) * (7+3-1)
-- Some example
sol01 = App Mul 
    (App Add (App Add (Val 50) (Val 25)) (Val 10))
    (App Sub (App Add (Val 7) (Val 3)) (Val 1))
numbers = [1,3,7,10,25,50] ::[Int]
target = 765




-- ## Brute Force Solution

-- Return a list of all possible ways of splitting a list into two non-empty parts
split :: [a] -> [([a], [a])]
-- Example : split [1,2,3,4] => [([1],[2,3,4]), ([1,2],[3,4]), ([1,2,3],[4])]

-- My own take using splitAt
split xs = run (length xs-1) xs where
  run 1 xs = [splitAt 1 xs]
  run n xs = splitAt n xs : run (n-1) xs

-- TODO: My Own take using take & drop & list comprehension


-- Return a list of all possible expressions whose values are precisely
-- a given list of numbers this is the key function in this lecture:
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [ e | (ls, rs) <- split ns
                  , l <- exprs ls
                  , r <- exprs rs
                  , e <- combine l r]
          where combine l r = [App o l r | o <- [Add, Mul, Sub, Div]]



solutions :: [Int] -> Int -> [Expr]
solutions ns t = [ e | ns' <- choices ns
                      ,e <- exprs ns'
                      ,eval e == [t]]


-- ## How Fast Is It ?

-- Quite slow actually.

-- System:        2.8GHz Core 2 Duo, 4GB RAM
-- Compiler:      GHC version 7.10.2
-- Example :      solutions [1,3,7,10,25,50] 765 
-- One solution:  0.108 seconds
-- All solutions: 12.224 seconds

-- ## Can We Do Better?

-- * Many of the expressions that are considered will typically be invalid - fail to evaluate.
-- * For our example, only around 5 million of the 33 million possible expressions are valid
-- * Combining generation with evaluation would allow "earlier rejection" of invalid expressions.


-- ## Fusing Two Functions

-- Valid expressions and their values:
type Result = (Expr, Int)

-- We seek to define a function that fuses together the generation and evaluatiçon of expressions:
-- results :: [Int] -> [Result]
-- results ns = [(e,n) | e <- exprs ns, n <- eval e]
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [ res | (ls, rs) <- split ns
                  , l <- results ls
                  , r <- results rs
                  , res <- combine l r]
          where combine (l,x) (r,y) = [(App o l r, apply o x y) | o <- [Add, Mul, Sub, Div], valid o x y]



solutions' :: [Int] -> Int -> [Expr]
solutions' ns t = [ fst r | ns' <- choices ns
                      , r <- results ns'
                      , snd r == t]

-- ## How Fast Is It Now
-- This approach is around 10 times faster.


-- ## Can We Do Better
-- * Many expressions will be essentially the same using simple arithmetic properties, such as:
        -- x*y = y*x
        -- x*1 = x

-- * Exploiting such properties would considerably reduce the search and solution spaces.


-- ## Exploiting Properties

-- Strengthening the valid predicate to take account of commutativity and identity properties:
valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x <= y && x /= 1 && y /= 1
valid' Div x y = x `mod` y == 0 && y /= 1

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n, n) | n > 0]
results' ns = [ res | (ls, rs) <- split ns
                  , l <- results' ls
                  , r <- results' rs
                  , res <- combine l r]
          where combine (l,x) (r,y) = [(App o l r, apply o x y) | o <- [Add, Mul, Sub, Div], valid' o x y]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns t = [ fst r | ns' <- choices ns
                      , r <- results' ns'
                      , snd r == t]

-- With this latest approach we only have 250 000 valid expressions (Around 20 times less) and 49 expressions (Around 16 times less)
-- Overall, this solution is around 100 times faster than the original version.