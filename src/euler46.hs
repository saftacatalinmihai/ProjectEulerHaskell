module Euler46 where
import Data.List
import Data.Maybe
import Utils
import Primes

main = print test

test = Just 5777 == firstNonGoldbach

composites = [ c | c <- map (\i -> 2 * i + 1) [1..], not (isPrime c) ]

goldbachHolds n =
    isJust $ find (==n) [ p + s |
        p <- takeWhile (<=(n - 2)) primesTD,
        s <- takeWhile (\sq -> p + sq <= n) [ 2 * i * i | i <- [1..]]
    ]

firstNonGoldbach = find (\x -> not $ goldbachHolds x )  composites
