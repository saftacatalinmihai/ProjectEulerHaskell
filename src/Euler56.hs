module Euler56 where
import Utils

main = print test

test = 972 == maximum ( map ( sum . digits ) [ i^j | i <- [1..100], j <- [1..100] ] )
