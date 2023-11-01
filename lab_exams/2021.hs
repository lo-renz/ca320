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

-- Q3
-- Write a function, join, that takes two lists and returns a list that is the concatenation of both lists except the elements in the
-- first list that are already in the second list are not added to the resulting list, e.g. join [1,2,3] [2,4,6,8] evaluates to
-- [1,3,2,4,6,8]
join :: [Int] -> [Int] -> [Int]
join [] ys = ys
join (x:xs) ys
    | x `elem` ys = join xs ys
    | otherwise = x : join xs ys
