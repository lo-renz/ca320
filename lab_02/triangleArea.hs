triangleArea :: Float -> Float -> Float -> Float
--s :: Float -> Float -> Float -> Float
--triangleArea a b c = sqrt((s*(s - a)) * (s - b) * (s - c)) | s = (a+b+c)/2
--triangleArea a b c = s == (a+b+c)/2 | sqrt((s*(s - a)) * (s - b) * (s - c))
triangleArea a b c = sqrt((((sum [a,b,c])/2)*(((sum [a,b,c])/2) - a)) * (((sum [a,b,c])/2) - b) * (((sum [a,b,c])/2) - c))
--s a b c = (sum [a,b,c]) / 2
