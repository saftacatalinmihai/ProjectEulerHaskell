module Main where
    import System.Random

    main = print "Hello"

    insertAllIdx x xs =
        map (\i -> (take i xs) ++ [x] ++ (drop i xs) ) [0.. length xs]

    perm (x:y:[]) = [[x,y],[y,x]]
    perm (x:xs) = concatMap (\p -> insertAllIdx x p) (perm xs)

    matrix = do
        g <- newStdGen
        print $ (randomRs (0 :: Int , 1) g)