isSum :: Int -> Int -> Int -> Bool
isSum a b c = if a + b == c
                  then True
              else if b + c == a
                  then True
              else if a + c == b
                  then True
              else False
