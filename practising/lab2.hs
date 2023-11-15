{- Exercise 1: Area of a Triangle -}
-- triangleArea :: Float -> Float -> Float -> Float
-- triangleArea a b c = sqrt(s*(s-a)*(s-b)*(s-c))
--     where s = (a + b + c)/2

{- Exercise 2: Sum Test -}
isSum :: Int -> Int -> Int -> Bool
isSum a b c
    | a + b == c = True
    | b + c == a = True
    | a + c == b = True
    | otherwise = False

{- Exercise 3: Area of a Triangle Revisted -}
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c
    | (a + b) > c && (b + c) > a && (a + c) > b = sqrt(s*(s-a)*(s-b)*(s-c))
    | otherwise = error "Not a triangle!"
     where s = (a + b + c)/2

main :: IO()
main = do
    putStrLn "----------"

    putStrLn "triangleArea 3 4 5"
    putStrLn (show (triangleArea 3 4 5))

    putStrLn "----------"

    putStrLn "isSum 1 2 3"
    putStrLn (show(isSum 1 2 3))
    putStrLn "isSum 4 5 9"
    putStrLn (show(isSum 4 5 9))
    putStrLn "isSum 12 5 7"
    putStrLn (show(isSum 12 5 7))
    putStrLn "isSum 23 23 23"
    putStrLn (show(isSum 23 23 23))

    putStrLn "----------"

    putStrLn "triangleArea 1 2 4"
    putStrLn (show(triangleArea 1 2 4))
