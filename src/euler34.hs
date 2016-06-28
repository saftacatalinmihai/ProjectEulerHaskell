module Euler34 where
import Utils

main = print test

test = 40730 == sum ( filter isCurious [1..99999] ) - 3

fact x = product [1..x]

isCurious x =
    x == (sum (map fact (digits x)))
