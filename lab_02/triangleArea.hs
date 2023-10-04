triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt((((sum [a,b,c])/2)*(((sum [a,b,c])/2) - a)) * (((sum [a,b,c])/2) - b) * (((sum [a,b,c])/2) - c))
