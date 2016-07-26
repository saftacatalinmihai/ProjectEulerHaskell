module Euler57 where
import Utils

main = print test

test = 153 == length
                ( filter
                    (\(x,y) -> (length . digits) x > ( length . digits) y )
                    get_fractions )

next_fraction (x, y) =
    (sum_x_y + y, sum_x_y)
    where
        sum_x_y = x + y

get_fractions_iter acc 1000 = acc
get_fractions_iter (fraction : acc ) i =
    get_fractions_iter ((next_fraction fraction) : (fraction : acc )) (i + 1)

get_fractions = get_fractions_iter [(3,2)] 0