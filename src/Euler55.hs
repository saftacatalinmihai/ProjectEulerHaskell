module Euler55 where
import Data.List
import Data.Ord
import Data.Maybe
import Utils

(|>) x f = f x

main = print test

test = 249 == length (filter is_lychrel [1..10000])

is_lychrel_iter _ 50 = True
is_lychrel_iter (_, True) _ = False
is_lychrel_iter (x, _) i = is_lychrel_iter (iter num) (i + 1)

is_lychrel x = is_lychrel_iter (iter x) 0

iter x =
    (nextNum, isPalindromic nextNum)
    where
        nextNum = x + ( numFromDigits $ digits x )

isPalindromic x =
    digits_x == (reverse $ digits_x)
    where
        digits_x = digits x