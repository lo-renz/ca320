-- Q1
-- Write an expression that evaluates to a list of the odd numbers that are a multiple of 3 and less than 100.
oddMultiplesOf3 :: [Int]
oddMultiplesOf3 = [x | x <- [1..100], x `mod` 2 == 0, x `mod` 3 == 0]

-- Q2
-- Write a function that takes two integers and evaluates to their sum if either integer is odd, otherwise evaluates to their difference.
sumOrDiff :: Int -> Int -> Int
sumOrDiff x y = if x `mod` 2 == 1 || y `mod` 2 == 1
                  then x + y
                else x - y
