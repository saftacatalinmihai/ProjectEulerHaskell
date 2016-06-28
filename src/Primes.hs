module Primes ( primesTD, isPrime ) where
noDivs n fs = foldr (\f r -> f*f > n || (rem n f > 0 && r)) True fs

primesTD = 2 : 3 : filter (`noDivs` tail primesTD) [5,7..]

isPrime n = n > 1 && noDivs n primesTD
