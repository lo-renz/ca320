isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome x =
    if x == reverse x
        then True
    else False