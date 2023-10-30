type Poly = [Float]
sumPolys :: Poly -> Poly -> Poly
sumPolys [] p = p
sumPolys p [] = p
sumPolys (p:ps) (q:qs) = (p+q):(sumPolys ps qs)