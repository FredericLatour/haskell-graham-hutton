module Main where

import Test.HUnit
import Fp13TheCountdownProblem

main :: IO ()
main = do 
        -- print $ solutions [1,3,7,10,25,50] 765
        print "optimized solution"
        print "******************"
        print $ solutions' [1,3,7,10,25,50] 765
        print "optimized solution some more"
        print "****************************"
        print $ solutions'' [1,3,7,10,25,50] 765
