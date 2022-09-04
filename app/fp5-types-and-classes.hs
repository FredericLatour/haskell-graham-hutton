add :: (Int, Int) -> Int
add (x,y) = x+y

zeroto :: Int -> [Int]
zeroto n = [0..n]


add' :: Int -> (Int -> Int)
add' x y = x+y

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

-- Exercices 1
e1 = ['a','b','c']  -- [char]
e2 = ('a','b','c')  -- (char, char, char)
e3 = [(False, '0'), (True, '1')] -- [(Bool, char)]
e4 = ([False, True], ['0', '1']) -- ([Bool], [char])
e5 = [tail, init, reverse] -- [[a] -> [a]]

-- Exercices 2
second xs = head (tail xs) -- [a] -> a
swap (x,y) = (y,x) -- (a, b) -> (b, a)
pair x y = (x,y) -- a -> b -> (a, b)
double x = x*2 -- Num a = a -> a
palindrome xs = reverse xs == xs -- Eq a => [a] -> Bool
twice f x = f (f x) -- (a -> a) -> a -> a


