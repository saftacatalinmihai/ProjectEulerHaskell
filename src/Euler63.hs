module Euler63 where
import Utils

main = print test

test = 49 == sol

sol =
    length $
    [(i,j) |
        i <- [1..9],
        j <- (takeWhile
                (\j -> j <= ( length $ digits ( i^j ) ) )
                [1..])
    ]