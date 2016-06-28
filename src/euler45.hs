module Euler45 where
import Data.List
import Utils

main = print test

test = 1533776805 == triangAndPentAndHex !! 2

triangAndPentAndHex = filter (\x -> ( isPent x ) && (isHex x) ) triangles

triang n = n * ( n + 1 ) / 2
pent n   = n * ( 3 * n - 1 ) / 2
hex n    = n * ( 2 * n - 1 )

triangles   = map ( round . triang) [1..]
pentagonals = map ( round . pent) [1..]
hexagonals  = map hex [1..]

isPent n =
    isInt ( ( sqrt (1 + 24 * (fromIntegral n)) + 1 ) / 6)

isHex n =
    isInt ( ( sqrt (1 + 8 * (fromIntegral n)) + 1 ) / 4)

isInt x = x == fromInteger (round x)