module Euler44 where
import Data.List
import Utils
import Debug.Trace
debug = flip trace

main = print test

-- takes a verry long time
test = 5482660 == pk - pj
    where
        (pj, pk) = head pSumDif

p n = n * (3 * n - 1) / 2

pentagonalNums =  map (round . p) [1..]

pPairs =
    concatMap ( \x -> reverse $ map (\y -> (y, x)) $ takeWhile (<x) pentagonalNums ) pentagonalNums

pSumDif =
    filter (\(pj, pk) ->
        isPentagonal (pj + pk) && isPentagonal (pk - pj)  ) pPairs

isPentagonal n =
    isInt ( ( sqrt (1 + 24 * (fromIntegral n)) + 1 ) / 6)

isInt x = x == fromInteger (round x)