module Euler64 where
import Utils
import Debug.Trace

main = print test

test = 1 == 1

a0 sq = floor $ sqrt sq

x |> f = f x

next_frac :: Int -> (Int, Int, Int) -> (Int, (Int, Int, Int))
next_frac a0 (a, sq, b) =
    (na, (next_a, sq, next_b))
    where
        f1 = (sq - (b * b))
        next_a = f1 `div` a
        na = ((a0 + b) `div` next_a)
        next_b = na * next_a - b

accumulate a0 a sq b acc | trace ("accumulate " ++ show acc ++ " " ++ show a ++ " " ++ show sq ++ " " ++ show b ) False = undefined
accumulate a0 a sq b acc
    | (loop_found acc) = acc
    | otherwise = accumulate a0 na sq nb (a1:acc)
    where
        (a1, (na, sq, nb)) = next_frac a0 (a, sq, b)

split l = splitAt (((length l) + 1) `div` 2) l
loop_found l =
    ((length a) > 0) && a == b
    where
        (a,b) = split l