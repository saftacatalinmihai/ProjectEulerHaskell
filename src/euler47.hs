module Euler47 where
import Data.List
import Data.Maybe
import Utils

main = print test

test = 134043 == first
    where
        (first, sec, thrd, fourth) = sol

numFactors n = length $ nub $ factors n

sol =
    head $ filter
        (\(a,b,c,d) -> all (\x -> 4 == numFactors x) [a,b,c,d])
        [(a,b,c,d) | a <- [1..], let b = a + 1, let c = b + 1, let d = c + 1]