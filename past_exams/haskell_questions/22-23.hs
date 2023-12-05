{- Question 1 -}

-- 1a: Describe list comprehension in the Haskell Programming language using examples.
-- List comprehension in the Haskell Programm language is the implmentation of set notation i nmathematics. It is used to quickly generate a list of elements. It may also make use of constraints, e.g. only add even numbers.
-- An example of a list comprehension which generates a list of even numbers from 1 to 100 can be written as follows:
evenNumbers = [x | x <- [1 .. 100], x `mod` 2 == 0]

-- 1b: Write a Haskell function called vowels that takes a string and using list comprehension returns a string containing all the vowels from the original string. Include the function type for vowels. For example, vowels "Hello World" would evaluate to "eoo".
vowels :: String -> String
vowels s = [c | c <- s, c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u']

-- 1c: Without using the list concatenation operator ++, write a Haskell function that takes a list of lists as input and returns a list that is the concatenation of all the original lists. For example, if the input to the function was [[3,2,1], [8,9], [1,2,3,4]], the function would evaluate to [3,2,1,8,9,1,2,3,4].
-- Appartenly must use 'foldr' function
listConcat :: [[a]] -> [a]
listConcat [] = []
listConcat (x : xs) = x ++ listConcat xs
