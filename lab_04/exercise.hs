-- First Part of the lab.

-- Append two lists.
-- Exam solution with Recursion
{-myAppend :: [a] -> [a] -> [a]
myAppend x [] = x
myAppend [] x = x
myAppend (x:xs) y = x:(myAppend xs y)-}

-- My implementation
myAppend :: [a] -> [a] -> [a]
myAppend [] x = x
myAppend x y = x ++ y

-- Extract the first element of a list, which must be non-empty.
myHead :: [a] -> a
myHead [] = error "Empty list."
--myHead (x:xs) = x
myHead x = x !! 0

-- Extract the last element of a list, which must be finite and non-empty.
myLast :: (Ord a) => [a] -> a
myLast (x:xs)
    | length xs == 0 = x
    | otherwise = myLast xs

--myLast x = x !! ((length x) - 1)

-- Extract the elements after the head of a list, which must be non-empty.
myTail :: [a] -> [a]
myTail [] = error "Empty list."
myTail (x:xs) = xs

-- Return all the elements of a list except the last one. The list must be non-empty.
myInit :: (Ord a) => [a] -> [a]
myInit [] = error "Empty list."
myInit [x] = []
myInit (x:xs) = x:(myInit xs)

-- Return the length of a finite list as an Int.
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

-- Return the elements of xs in reverse order. xs must be finite.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Concatenate a list of lists.
myConcat :: [[a]] -> [a]
myConcat [x] = x
myConcat (x:xs) = x ++ (myConcat xs)

-- Computes the sum of a finite list of numbers.
mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

-- Computes the product of a finite list of numbers.
myProduct :: (Num a) => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * (myProduct xs)

-- Returns the maximum value from a list, which must be non-empty, finite and of an ordered type.
myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "Empty list."
myMaximum [x] = x
myMaximum (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = myMaximum xs

-- Returns the minimum value from a list, which must be non-empty, finite, and of an ordered type.
myMinimum :: (Ord a) => [a] -> a
myMinimum [] = error "Empty list."
myMinimum [x] = x
myMinimum (x:xs)
    | x < minTail = x
    | otherwise = minTail
    where minTail = myMinimum xs

-- The list membership predicate
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
    | x == y = True
    | otherwise = myElem x ys

-- Removes the first occurence of x from its list argument.
myDelete :: (Eq a) => a -> [a] -> [a]
myDelete _ [] = []
myDelete x (y:ys)
    | x == y = ys
    | otherwise = y:(myDelete x ys)

-- Second Part of the lab
-- returns the list union of the two lists. Duplicates, and elements of the first list, are removed from the
-- second list, but if the first list contains duplicates, so will the result. . For example,
-- myUnion [1,3,5,1] [2,2,3,4] == [1,3,5,1,2,4]

myUnion :: (Eq a) => [a] -> [a] -> [a]
myUnion x [] = x
myUnion x (y:ys)
    | myElem y x = myUnion x ys
    | otherwise = myUnion (x++[y]) ys

-- returns the list intersection of two lists. For example,
-- myIntersect [1,2,3,4] [2,4,6,8] == [2,4]
-- If the first list contains duplicates, so will the result.
-- myIntersect [1,2,2,3,4] [6,4,4,2] == [2,2,4]
myIntersect :: (Eq a) => [a] -> [a] -> [a]
myIntersect [] _ = []
myIntersect (x:xs) y
    | myElem x y = x:(myIntersect xs y)
    | otherwise = myIntersect xs y
