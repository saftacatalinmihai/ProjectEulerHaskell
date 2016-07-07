module Euler52 where
import Data.List
import Data.Ord
import Data.Maybe
import Utils
import Primes

main = print test

test = Just 142857 == find sol [1..]

sol x =
    d == d2 && d == d3 && d == d4 && d == d5 && d == d6
    where
    d  = sort $ reverse $ digits x
    d2 = sort $ reverse $ digits (x * 2)
    d3 = sort $ reverse $ digits (x * 3)
    d4 = sort $ reverse $ digits (x * 4)
    d5 = sort $ reverse $ digits (x * 5)
    d6 = sort $ reverse $ digits (x * 6)