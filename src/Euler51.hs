module Euler51 where
import Data.List
import Data.Ord
import Data.Maybe
import Utils
import Primes

main = print test

test = Just [121313,222323,323333,424343,525353,626363,828383,929393] == findSolForLength 8

(|>) x f = f x

replace inNum by atPositions =
    numFromDigits $ nDigs `zip` [0..]
    |> map (\(d, pos) -> if pos `elem` atPositions then by else d)
    where nDigs = reverse $ digits inNum

positionsToChange inNum =
    subsequences pos |> drop 1 |> dropLast
    where
        pos = [0.. (length $ digits inNum) - 1]

dropLast l =
    take (length l - 1) l

numToChangeByAtPos pos inNum
    | elem (length digs - 1 ) pos = map (\x -> x*2 -1 ) [1..5]
    | elem 0 pos = [1..9]
    | otherwise = [0..9]
    where digs = digits inNum

posAndNumToCheck num =
    map (\p -> (p, numToChangeByAtPos p num)) $ positionsToChange num

numsToCheck num =
    map (\(p, numList) -> map (\n -> replace num n p) numList ) $ posAndNumToCheck num

primeListFor num =
    maximumBy ( comparing length) $ filter ( /=[] ) $ map (\l -> filter isPrime l ) $ numsToCheck num

findSolForLength n =
    find (\l -> length l == n) $ map primeListFor $ drop 4 primes