listLength :: (Integral b) => [a] -> b
listLength [] = 0
listLength (_:xs) = 1 + listLength xs
