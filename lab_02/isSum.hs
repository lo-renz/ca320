isSum :: Int -> Int -> Int -> Bool
isSum x y z
    | (x+y == z) = True
    | (y+z == x) = True
    | (x+z == y) = True
    | otherwise = False
