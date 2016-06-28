module Euler48 where
import Data.List
import Utils

main = print test

test = "9110846700" == sol

series = [ i ^ i | i <- [ 1..]]
seriesTo n = sum $ take n series

sol =
    drop (length nStr - 10) nStr
    where
        nStr = show $ seriesTo 1000
