myAppend :: [a] -> [a] -> [a]
myAppend [] x = x
myAppend (x:xs) y = x:(myAppend xs y)

myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (x:_) = x

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myTail :: [a] -> [a]
myTail [] = error "Empty list"
myTail (_:xs) = xs

myInit :: [a] -> [a]
myInit [] = error "Empty list"
myInit [x] = []
myInit (x:xs) = x:(myInit xs)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength(xs)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- myConcat doesn't work
myConcat :: [a] -> [a] -> [a]
myConcat [] [y] = [y]
myConcat [x] [] = [x]
myConcat (x:xs) y = x:(myConcat xs y)

mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

myProduct :: (Num a) => [a] -> a
myProduct [x] = x
myProduct (x:xs) = x * (myProduct xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "Empty list"
myMaximum [x] = x
myMaximum (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = myMaximum xs

myMinimum :: (Ord a) => [a] -> a
myMinimum [] = error "Empty list"
myMinimum [x] = x
myMinimum (x:xs)
    | x < minTail = x
    | otherwise = minTail
    where minTail = myMinimum xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = if x == y
                    then True
                  else (myElem x ys)

myDelete :: (Eq a) => a -> [a] -> [a]
myDelete _ [] = []
myDelete x (y:ys) = if x == y
                      then ys
                    else y:(myDelete x ys)