myAppend :: [a] -> [a] -> [a]
myAppend x [] = x
myAppend [] y = y
myAppend (x:xs) ys = x : (myAppend xs ys)

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat [x] = x
myConcat (x:xs) = x ++ myConcat xs

myMaximum :: (Ord a) => [a] -> a
myMaximum [x] = x
myMaximum (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = myMaximum xs
