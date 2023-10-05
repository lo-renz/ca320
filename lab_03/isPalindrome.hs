isPalindrome :: [a] -> Bool
isPalindrome [] = True
isPalindrome [a] = if reverse [a] == [a]
                    then True
                    else False
