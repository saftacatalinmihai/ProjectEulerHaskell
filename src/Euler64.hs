module Euler64 where
import Utils
import Debug.Trace

main = print test

test = 1322 == odd_period_count_to 10000

a_0 sq = floor $ sqrt ( fromIntegral sq ) 

x |> f = f x

-- next_frac :: Int -> Int ->(Int, Int) -> (Int, (Int, Int, Int))
next_frac a0 sq (a, b) =
    (na, (next_a, sq, next_b))
    where
        f1 = (sq - (b * b))
        next_a = f1 `div` a
        na = ((a0 + b) `div` next_a)
        next_b = na * next_a - b

-- accumulate a0 a sq b acc | trace ("accumulate " ++ show acc ++ " " ++ show a ++ " " ++ show sq ++ " " ++ show b ) False = undefined
accumulate a0 a sq b acc
    | (loop_found acc) = acc
    | otherwise = (accumulate a0 na sq nb ((a1,na,nb):acc))
    where
        (a1, (na, _, nb)) = (next_frac a0 sq (a, b))

split l = splitAt (((length l) + 1) `div` 2) l
loop_found l =
    ((length a) > 0) && a == b
    where
        (a,b) = split l

continued_frac sq =
   (a,repeating_seq)
        where
        a = (a_0 sq)
        l = accumulate a 1 sq a []
        (repeating_seq, _) = split l

period n = 
    length l
    where
        (a0, l) = continued_frac n

ints_without_squares n =
    [ x | x <- [1..n], x `notElem`  [ y*y | y <- [1..(floor $ sqrt (fromIntegral n))]] ]

odd_period_count_to n = length $ filter (\x -> (x `rem` 2) == 1 ) $ map period ( ints_without_squares n)

