module Euler53 where
import Data.List
import Data.Ord
import Data.Maybe
import Utils
import Primes

main = print test

test = sol == 4075

fact n = product [2..n]

comb n r =
    fact n `div` ((fact r) * (fact (n - r)))

sol = length $ filter (>1000000) $ concatMap (\n -> map (\r -> comb n r) [1..n]) [1..100]