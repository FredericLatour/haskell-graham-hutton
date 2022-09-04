double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

last' xs = head $ reverse xs
last'' xs = xs !! (length xs - 1)
last''' xs = drop (length xs - 1) xs

init' xs = take (length xs - 1) xs
init'' xs = reverse $ tail $ reverse xs
init''' xs = reverse (drop 1 $ reverse xs)

