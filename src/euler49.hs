module Euler49 where
import Data.List
import Data.Maybe
import Utils
import Primes

main = print test

test = (2969,6299,9629) == sols !! 1

sols = filter arePerm primeTriplets

arePerm (a,b,c) =
    elem (digits b) allPerm && elem (digits c) allPerm
    where
        allPerm = permutations $ digits a

primeTriplets =
    filter (\(a,b,c) -> all isPrime [a,b,c]) triplets

triplets = concatMap tripletsFrom $ primesFromTo 1000 9999

tripletsFrom i =
    takeWhile (\(a,b,c) -> c < 9999)
        [ (i, i + a, i + 2 * a ) | a <- [2..]]

primesFromTo from to = filter (>=from) $ takeWhile (<=to) primesTD

