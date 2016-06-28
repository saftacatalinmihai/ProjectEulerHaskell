module Euler36 where
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

main = print test

test = 872187 == (sum $ palindromesBase10and2Bellow 1000000)

binaryStr n = showIntAtBase 2 intToDigit n ""

isPalindrome s = s == reverse s

palindromesBase10and2Bellow n =
    filter (\x ->
        (isPalindrome (show x)) &&
        (isPalindrome (binaryStr x)) )
        [1..n]