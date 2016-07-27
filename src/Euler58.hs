module Euler58 where
import Primes

main = print test

test = 26241 == find_first

next_corner_nums (side_l, (h:rest), primes) =
    (next_side_l, corner_nums ++ (h:rest), just_primes ++ primes)
    where
        next_side_l = side_l + 2
        corner_nums = [h + 4 * next_side_l,
                            h + 3 * next_side_l,
                            h + 2 * next_side_l,
                            h + next_side_l ]
        just_primes = filter isPrime corner_nums

prime_ratio (side_l, _, primes )  =
    (realToFrac $ length primes) / (realToFrac (side_l * 2 +  1) )

corner_nums = (2, [9,7,5,3], [7,5,3]) : map (\cn -> next_corner_nums cn)  corner_nums

find_first =
    l + 1
    where
        (l, _, _) = head $ dropWhile (\l -> (prime_ratio l) > 0.1) corner_nums