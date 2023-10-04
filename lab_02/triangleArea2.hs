triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c
    | isTriangle a b c = sqrt (((s * (s - a)) * (s - b) * (s - c)))
    | otherwise = error "Not a triangle!"
  where
    s = (a + b + c) / 2

isTriangle :: Float -> Float -> Float -> Bool
isTriangle a b c = a + b > c && a + c > b && b + c > a

