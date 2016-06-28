module Euler41 where
import Data.List
import Utils
import Primes

main = test

test = 7652413 == head pandigitalPrimes

pandigitalPrimes =  filter (\p -> isPandigital p && (isPrime ( toInteger p ) ) ) $ reverse [1 .. 7654321]
