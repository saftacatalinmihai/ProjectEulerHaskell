module Utils (digits, numFromDigits, isPandigital, factors, primes) where
import Data.List

digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

numFromDigits l = foldl (\x acc ->  x * 10 + acc ) 0 l

isPandigital x =
    [1..length d] == sort d
    where
        d = digits x

factors m = f m (head primes) (tail primes) where
  f m n ns
    | m < 2 = []
    | m < n ^ 2 = [m]   -- stop early
    | m `mod` n == 0 = n : f (m `div` n) n ns
    | otherwise = f m (head ns) (tail ns)

primes = 2 : filter (\n-> head (factors n) == n) [3,5..]