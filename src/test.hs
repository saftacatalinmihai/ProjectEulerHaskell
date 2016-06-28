import Data.List
coins = [1,2,5,10,20,50,100,200]
coin = 200
weights = replicate (length coins) 0

sumCoinsWeights coins weights =
    sum ( zipWith (*) coins weights )

sumCoins = sumCoinsWeights coins

isSingleNonZeroInTheMiddle xs =
    length m == 1 && length b > 0 && a ++ m ++ b == xs
    where
        a = takeWhile (==0) xs
        b = takeWhile (==0) (reverse xs)
        m = filter (/=0) xs

incLastNonZero xs =
    (map (\x -> 0) firstPart) ++ [inremented] ++ zeros
    where
        (x:firstPart) =  dropWhile (==0) (reverse xs)
        inremented = x + 1
        zeros = takeWhile (==0) (reverse xs)

incNextWeight xs =
    0: a ++ [1] ++ (tail b)
    where
        a = takeWhile (==0) xs
        b = takeWhile (==0) (reverse xs)

nextWeights coin coins weights
    | sumOfCoins < coin = head weights  + 1 : tail weights
    | sumOfCoins >= coin = if isSingleNonZeroInTheMiddle weights
                               then incNextWeight weights
                               else incLastNonZero weights
    where sumOfCoins = sumCoinsWeights coins weights

--countComb :: Int -> [Int] -> [Int] -> Int
--countComb coin coins weights acc
--    | sumOfCoins == coin && weights == (replicate (length weights - 1) 0 ++ [1]) = acc + 1
--    | sumOfCoins == coin = (countComb coin coins (nextWeights coin coins weights) (acc + 1))
--    | otherwise = (countComb coin coins (nextWeights coin coins weights) acc)
--    where sumOfCoins = sumCoinsWeights coins weights

--memoizedCountChange = (map countChange [0 ..]  !!)
--    where
countChange money coins
    | money == 0 = 1
    | null coins || money < 0 = 0
    | otherwise = (countChange (money - (head coins)) coins) + (countChange money (tail coins))
--oneNonZero (x:xs) = length xs == (length (filter (==0) xs) + 1)

--isFound coin coins weights =
--    coin == sumCoinsWeights coins weights

--weightIdx weights idx = weights !! (idx - 1)
--allWeights = [[a,b,c,d,e,f,g,h] | a <- [0,1], b <- [0,2], c <- [0..4], d <- [0..10], e <- [0..20], f <- [0..40], g <- [0..100],h <- [0..200], isFound coin coins [a,b,c,d,e,f,g,h]]

incWeightsAt idx weights =
    firstPart ++ [(x + 1)] ++ lastPart
    where
        firstPart = take idx weights
        (x:lastPart) = drop idx weights

idxToInc coin coins weights =
   find (\i -> (sumCoinsWeights coins (incWeightsAt i weights)) <= coin ) [0 .. length weights]