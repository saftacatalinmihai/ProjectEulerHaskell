module Euler38 where
import Utils
import Data.List

main = print test

test = 932718654 == maximum pandigitalConcats

concatenation :: Int -> [Int] -> Int
concatenation n l = read $ intercalate "" $ map show $ map (\x -> x * n) l

intList = map (\x -> [1..x]) [1..]

pandigitalConcats =
    filter isPandigital $
    filter (>=100000000) $
    concatMap (\x ->
        map (\intL -> concatenation x intL) $
        takeWhile (\intL -> 999999999 >= concatenation x intL ) intList
    ) [1..10000]