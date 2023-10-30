{-shortest :: [[a]] -> [a]
shortest [] = []
shortest [x] = x
shortest (x:xs)
    | length x < tail = x
    | otherwise = shortest tail
    where tail = length (shortest xs)-}

shortest :: [[a]] -> [a]
shortest [] = []
shortest [x] = x
shortest (x:xs) = if length x < length (shortest xs)
                      then x
                  else shortest xs