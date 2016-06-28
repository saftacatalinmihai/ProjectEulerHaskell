module Euler39 where
import Data.List
main = print test

test = 840.0 == perim
    where
        (perim, solutions ) = findMaxSolsTo 1000

perim a b = a + b + (sqrt (a^2 + b^2))

solveP p =
    filter
        ( \(a,b,pf) -> pf == p )
        [ (a, b, perim a b) | a <- [1..p] , b <- [a..p] ]

findMaxSolsTo max =
    maximumBy
        ( \(a, al) (b, bl) -> compare (length al) (length bl) ) $
        map ( \x -> (x, solveP x) )
        [1..max]
