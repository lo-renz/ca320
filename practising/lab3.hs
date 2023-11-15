{- Exercise 1: Palindromes -}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome x
    | myReverse x == x = True
    | otherwise = False

{- Exercise 3: Shortest List -}
shortest :: (Ord a) => [[a]] -> [a]
shortest [] = []
shortest [x] = x
shortest (x:xs)
    | length x < length minTail = x
    | otherwise = minTail
    where minTail = shortest xs
