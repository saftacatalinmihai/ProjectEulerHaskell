module Euler43 where
import Data.List
import Utils

main = print test

test = sum pandigitalsWithProperty == 16695334890

hasSubstrProp (d1:d2:d3:d4:d5:d6:d7:d8:d9:d10:[])
    |   ((numFromDigits [d2, d3, d4]) `mod` 2) == 0 &&
        ((numFromDigits [d3, d4, d5]) `mod` 3) == 0 &&
        ((numFromDigits [d4, d5, d6]) `mod` 5) == 0 &&
        ((numFromDigits [d5, d6, d7]) `mod` 7) == 0 &&
        ((numFromDigits [d6, d7, d8]) `mod` 11) == 0 &&
        ((numFromDigits [d7, d8, d9]) `mod` 13) == 0 &&
        ((numFromDigits [d8, d9, d10]) `mod` 17) == 0 = True
    | otherwise = False

pandigitalsWithProperty =
    map numFromDigits $ filter hasSubstrProp $ permutations [0..9]

