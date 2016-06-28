module Euler33
( main ) where

import Data.List
import Data.Maybe

main = print test
test = 100 == denom prodOfDigitCanceling

fraction numer denom = (numer, denom)

numer f = n where (n,d) = f

denom f = d where (n,d) = f

prod f1 f2 =
    simplify ( fraction ( n1 * n2 ) (d1 * d2))
    where
        n1 = numer f1
        d1 = denom f1
        n2 = numer f2
        d2 = denom f2

simplify f =
    ( (numer f) `div` g , (denom f )`div` g)
    where
        g = gcd ( numer f) ( denom f)

allFractions =
   concatMap (\x -> map (\y -> fraction x y) [x + 1 .. 99]  )[11..99]

allNotTrivialFractions =
    filter (\f -> not (isTrivial f)) allFractions

naiveSimplify f
    | n1 == d1 = Just (fraction n2 d2)
    | n1 == d2 = Just (fraction n2 d1)
    | n2 == d1 = Just (fraction n1 d2)
    | n2 == d2 = Just (fraction n1 d1)
    | otherwise = Nothing
    where
        n = numer f
        d = denom f
        n1 = n `div` 10
        n2 = n `mod` 10
        d1 = d `div` 10
        d2 = d `mod` 10

fractionsWithNaiveSimplif =
    filter
        (\(f, m) -> isJust m)
        (map
            (\f -> (f ,(fmap simplify . naiveSimplify) f ) )
            allNotTrivialFractions)

isTrivial f =
    n `mod` 10 == 0 && d `mod` 10 == 0
    where
        n = numer f
        d = denom f

digitCanceling =
    filter (\(f1, Just f2) -> (simplify f1) == f2 ) fractionsWithNaiveSimplif

prodOfDigitCanceling =
    foldl prod (1,1) ((map (\(fr, m) -> fr )) digitCanceling )