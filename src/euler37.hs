module Euler37 where
import Primes
import Utils

main = print test

test = 748317 == sum truncatedPrimes

subset x dir from to =
    map numFromDigits $ map (\i -> dir i digs) [from .. to]
    where
        digs = reverse $ digits x

subsetRight x = subset x take 1 (length (digits x))
subsetLeft  x = subset x drop 0 (length (digits x) - 1)

isPrimeTruncated x =
     all isPrime $ subsetRight x ++ subsetLeft x

truncatedPrimes = take 11 $ filter isPrimeTruncated [11..]