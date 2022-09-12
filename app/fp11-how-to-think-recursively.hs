import Test.HUnit


-- [FP 11 - How to Think Recursively - YouTube](https://www.youtube.com/watch?v=n6bg8L91Qew)


-- What are Recursive Functions ?
--  It's a function defined in terms of itself.

-- Why are they useful ?
--  Many, if not most, functions have a simple and natural recursive definition .
--  Because in a language such as Haskell, recursion is the only way to do some kind of looping or repetition.
--  If you write your functions recursively, you can use math tools like induction to reason about your program.

-- Why are they difficult at first ?
--  It's a different way of thinking than imperative.
--  It's like riding a bike. It looks easy when someone is riding but is difficult at first. It becomes easy after some practice.



-- ## Problem :
-- define a function that calculates the sum of a list of numbers.

-- Seven stages process
  -- 1. Name the function
  -- 2. Write down its type. You can use a simple type first and generalize afterward.
  -- 3. Enumerate the cases. You look at the type of the arguments ([Int]) and you try standard cases. 
  --     You may have to refine the cases later on.
  -- 4. Define the simple cases. In this case `sum []` is the simple case. The simple cases are often the base cases.
  -- 5. List the "ingredients". We've got the function itself and the parameters and the target type.
  -- 6. Define the other cases. 
  -- 7. Think about the result. Can it be generalized. Can the definition be simplified.


-- sum :: [Int] -> Int => 7.
sum :: Num a => [a] -> a
-- sum []      = 0
-- sum (x:xs)  = x + sum xs  => 7. simplification
sum = foldr (+) 0

-- ## Problem :
-- Define a function that drops a given number of elements from the start of a list.
-- Use the 7 stage process.

drop :: Int -> [a] -> [a]
drop _ []     = []
drop 0 xs     = xs
drop n (_:xs) = Main.drop (n-1) xs


-- ## Problem :
-- define a function that removes the last element from a non-empty list.

init :: [a] -> [a]
-- init []     = []
init [_]    = []
init (x:xs) = x : Main.init xs


-- initial video definition
init' :: [a] -> [a]
init' (x:xs)  | null xs = []
              | otherwise  = x : Main.init' xs
