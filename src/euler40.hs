module Euler40 where
import Data.List
import Utils

main = print test

test = 	210 == prod

decimals = foldr(\x acc -> x ++ acc) "" $ map show [1..]

prod =
    product $ digits digs
    where
        d = take 1000000 decimals
        digs = read [ d !! 0, d !! 9, d !! 99, d !! 999,  d !! 9999, d !! 99999, d !! 999999] :: Int