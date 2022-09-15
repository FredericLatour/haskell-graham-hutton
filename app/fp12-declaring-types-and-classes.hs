import Test.HUnit


-- [FP 12 - Chapter 8 - Declaring Types and Class - YouTube](https://www.youtube.com/watch?v=sYgvpTyFpZ4)


-- In Haskell, a new name for an existing type can be defined using a "type declaration".
-- You define function but you declare type.

type String = [Char] -- String is a synonym for the type [Char] (from the Haskell standard library)

-- Type declarations can be used to make other types easier to read.
-- For example, given:
type Pos = (Int,Int)

-- we can define: 
origin :: Pos
origin = (0,0)

left :: Pos -> Pos
left (x,y) = (x-1, y)


-- Like function definitions, type declarations can also have parameters. 
-- For example, given

type Pair a = (a,a)

-- we can define: 
mult :: Pair Int -> Int
mult (m, n) = m*n

copy :: a -> Pair a
copy x = (x,x)


-- Type declarations can be nested:
type Trans = Pos -> Pos

-- However, they cannot be recursive:
-- type Tree = (Int, [Tree]) -- does not work


-- ## Data Declaration
-- ===================

-- A completely new type can be defined by specifying its values using a data declaration.

data Bool = False | True -- Bool is a new type with two values: False and True.


-- * The two values False and True are called the constructors for the type Bool. 
-- * Type and constructor names must always begin with an upper-case letter
-- * Data declarations are similar to context free grammars. The former specifies 
--   the values of a type, the latter the sentences of a language.

-- Values of new types can be used in the same ways as those of built in types.
-- For example, given
data Answer = Yes | No | Unknown deriving (Show)

-- We can define:
answers :: [Answer]
answers = [Yes,No,Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown


-- The constructors in a data declaration can also have parameters.
-- For example, given

data Shape = Circle Float | Rect Float Float deriving (Show)

-- we can define:
square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x*y

-- Note:
-- * Shape has values of the form Circle r where r is a float, and Rect x y  where x and y are floats.
-- * Circle and Rect can be viewed as functions that construct values of type Shape:
--    Circle :: Float -> Shape
--    Rect :: Float -> Float -> Shape
-- That's exactly how these are viewed internally (:t Circle or :t Rect).


-- Data declarations can have parameters. For example, given
data Maybe a = Nothing | Just a
-- we can define :
safediv :: Int -> Int -> Main.Maybe Int
safediv _ 0 = Main.Nothing
safediv m n = Main.Just (m `div` n)

safehead :: [a] -> Main.Maybe a
safehead [] = Main.Nothing
safehead (x:xs) = Main.Just x

-- ## Recursive Types
-- ==================

-- In Haskell, new types can be declared in terms of themselves. That is, types can be recursive.

data Nat = Zero | Succ Nat deriving (Show)

-- Nat is a new type with constructors
-- Zero :: Nat and Succ :: Nat -> Nat
-- Note:
-- * A value of type Nat is either Zero, or of the form Succ n where n :: Nat.
--   That is, Nat contains the following infinite sequence of values:
--    Zero
--    Succ Zero
--    Succ (Succ Zero)
--
-- * We can think of values of type Nas as natural numbers, where Zero represents 0,
--   and Succ represents the successor function 1+.
--
-- * For example, the value
--      Succ (Succ (Succ Zero))
--   represents the natural number
--      1 + (1 + (1 + 0)) = 3

-- Using recursion, it is easy to define functions that convert between values of type Nat and Int:
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- Two naturals can be added by converting them to integers, adding, and then converting back:
add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

-- However using recursion the function add can be defined without the need for conversions:
add' :: Nat -> Nat -> Nat
add' Zero n = n
add' (Succ m) n = Succ (add' m n)
-- For example:
-- add (Succ (Succ Zero)) (Succ Zero) =
-- Succ (add (Succ Zero) (Succ Zero)) =
-- Succ (Succ (add Zero) (Succ Zero)) =
-- Succ (Succ (Succ Zero))

-- Note:
-- the recursive defintion for add corresponds to the laws:
-- 0+n = n and 
-- (1+m)+n = 1+(m+n)



-- ## Arithmetic Expressions
-- =========================

-- Consider a simple form of expressions built up from integers 
-- using addition and multiplications

  --   [+]
  --  /   \
  -- [1]  [*]
  --     /   \
  --    [2]  [3]

-- Using recursion, a suitable new type to represent such expressions can be declared by:
data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr
          deriving (Show)

-- For example, the expression on the graph would be represented as follow
graph = Add (Val 1) (Mul (Val 2) (Val 3))

-- Using recursion, it is now easy to define functions that process
-- expressions. For example :

size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- Notes:
-- * The three constructors have types:
--    Val :: Int -> Expr
--    Add :: Expr -> Expr -> Expr
--    Mul :: Expr -> Expr -> Expr

-- Many functions on expressions can be defined by replacing the constructors by other
-- functions using a suitable `fold` function. For example:
--    eval = folde id (+) (*)


-- ## Exercices

-- 1. Using recursion and the function add, define a function that multiplies 
--    two natural numbers.
mult' :: Int -> Int -> Int
mult' 1 n = n
mult' m n = n + mult' (m-1) n

mult'' :: Nat -> Nat -> Nat
mult'' Zero n = Zero
mult'' (Succ Zero) n = n
mult'' (Succ m) n = mult'' m (add' n n)

-- Solution in the video
-- (n+1) * m = (n*m) + m
mult''' :: Nat -> Nat -> Nat
mult''' Zero m = Zero
mult''' (Succ n) m = add' (mult''' n m) m


-- 2. Define a suitable function `folde` for expressions and give a few example of its use.
--    I initially had a hard time to understand what was expected here.
folde :: (Int -> a) -> (a->a->a) -> (a->a->a) -> Expr -> a
folde id add mul (Val n) = id n
folde id add mul (Add x y) = add (folde id add mul x) (folde id add mul y)
folde id add mul (Mul x y) = mul (folde id add mul x) (folde id add mul y)


-- 3. Define a type `Tree a` of binary trees built from `Leaf` values of type `a` 
--    using a `Node` constructor that takes two binary trees as parameters.

data Tree a =  Leaf a
              | Node (Tree a) (Tree a)
              deriving (Show)

testTree :: Tree Int
testTree = Node (Leaf 4) (Node (Leaf 2) (Leaf 5))

