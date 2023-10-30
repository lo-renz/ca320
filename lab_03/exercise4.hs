type Poly = [Int]
evalPoly :: Poly -> Int -> Int
evalPoly [p] _ = p
evalPoly (p:ps) x = p + (x * (evalPoly ps x))