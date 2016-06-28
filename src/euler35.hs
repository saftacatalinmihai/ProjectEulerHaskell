module Euler35 where
import Data.List
import Utils
import Data.List (inits)
import Data.Array.Unboxed
import Primes

main = print test
test = 55 == circularPrimesBellow 1000000

--primesToMillion = takeWhile (<=1000000) primesSAE
--
--isPrime n = n > 1 &&
--              foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
--                True primesToMillion
--
--primesSAE = 2 : sieve 3 4 (tail primesSAE) (inits primesSAE)
--  where
--  sieve x q ps (fs:ft) = [i | (i,True) <- assocs (
--         accumArray (\ _ _ -> False)
--                    True  (x,q-1)
--                    [(i,()) | p <- fs, let c = p * div (x+p-1) p,
--                              i <- [c, c+p..q-1]] :: UArray Int Bool )]
--      ++ sieve q (head ps^2) (tail ps) ft

rot n =
    map numFromDigits [ take (length d) (drop i xs) | i <- [0 .. length d - 1] ]
    where
        d = reverse (digits n)
        xs = take (2 * length d - 1) (cycle d)

isCircularPrime x =
    all (==True) $ map isPrime $ rot x

circularPrimesBellow x =
    length . filter isCircularPrime $ [1..x]