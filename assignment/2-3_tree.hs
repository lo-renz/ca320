{-
 A 2-3 Tree which is either:
 - An empty tree

 - A node with 1 root element, a left subtree in which all the elements are less than or equal to the root element,
   and a right subtree in which all the elements are greater than the root element.

 - A node with 2 root elements, a left subtree in which all the elements are less than or equal to the first root element,
   a middle subtree in which all the elements are greater than the first root element and less than or equal to the second
   root element, and a right subtree in which all the elements greater than the second root element.
-}
data TwoThreeTree t = Empty
                    | TwoNode t (TwoThreeTree t) (TwoThreeTree t)
                    | ThreeNode t t (TwoThreeTree t) (TwoThreeTree t) (TwoThreeTree t)
                    deriving (Eq, Ord, Show)


-- Tree from assignment spec can be respresented as follows:
myTree1 = ThreeNode 3 10 Empty (TwoNode 6 Empty Empty) (ThreeNode 15 20 Empty (TwoNode 18 Empty Empty) Empty)

twoLeaf x = TwoNode x Empty Empty
threeLeaf x y = ThreeNode x y Empty Empty Empty

myThree2 = ThreeNode 3 10 Empty (twoLeaf 6) (ThreeNode 15 20 Empty (twoLeaf 18) Empty)

-- Implementation of add(X, T), which returns the 2-3 Tree from adding X to the 2-3 Tree T.
{- Explanation:
-}
add :: (Ord t) => t -> TwoThreeTree t -> TwoThreeTree t
add x Empty = twoLeaf x
add x (TwoNode y left right)
    | x < y     = ThreeNode x y left Empty right
    | otherwise = ThreeNode y x left Empty right
add x (ThreeNode y z left middle right)
    | x <= y          = ThreeNode y z (add x left) middle right
    | x > y && x <= z = ThreeNode y z left (add x middle) right
    | x > z           = ThreeNode y z left middle (add x right)
    | otherwise = error "Something wrong happened"

-- Implementation of member(X, T), which returns true if X is in the 2-3 Tree T.
{- Explanation:
-}
member :: (Ord t) => t -> TwoThreeTree t -> Bool
member x Empty = False
member x (TwoNode y left right)
    | x == y    = True
    | x < y = member x left
    | x > y = member x right
    | otherwise = False
member x (ThreeNode y z left middle right)
    | x == y    = True
    | x == z    = True
    | x <= y          = member x left
    | x > y && x <= z = member x middle
    | x > z           = member x right
    | otherwise       = False

-- Implementation of height(T), which returns the height of T.
height :: TwoThreeTree t -> Int
height Empty = -1
height (TwoNode x left right) = (max (height left) (height right)) + 1
height (ThreeNode x y left middle right) = (max (height left) (max (height middle) (height right))) + 1

-- A main function which shows off the implementation of the functions by printing the output of the functions to standard output.
main :: IO()
main = do
    let xList = [3, 10, 15, 20, 6, 18]

    -- To show off the implementation of add(X, T).
    -- Which returns the 2-3 Tree from adding X to the 2-3 Tree T.
    let t1 = Empty
    putStrLn (show (add (xList !! 0) t1))
    let t2 = add (xList !! 0) t1
    putStrLn (show (add (xList !! 1) t2))
    let t3 = add (xList !! 1) t2
    putStrLn (show (add (xList !! 2) t3))
    let t4 = add (xList !! 2) t3
    putStrLn (show (add (xList !! 3) t4))
    let t5 = add (xList !! 3) t4
    putStrLn (show (add (xList !! 4) t5))
    let t6 = add (xList !! 4) t5
    putStrLn (show (add (xList !! 5) t6))

    putStrLn (show ("----")) -- Just to separate the function prints.

    -- To show off the implementation of member(X, T).
    -- Which returns true if X is in the 2-3 Tree T.
    let t7 = add (xList !! 5) t6
    putStrLn (show (member 3 t7))
    putStrLn (show (member 10 t7))
    putStrLn (show (member 15 t7))
    putStrLn (show (member 20 t7))
    putStrLn (show (member 6 t7))
    putStrLn (show (member 18 t7))
    putStrLn (show (member 0 t7))
    putStrLn (show (member 100 t7))

    putStrLn (show ("----")) -- Just to separate the function prints.

    -- To show off the implementation of height(T).
    -- Which returns the height of the 2-3 Tree.
    let emptyTree = Empty
    putStrLn(show (height emptyTree))
    let binTree = TwoNode 5 (TwoNode 1 Empty (TwoNode 3 Empty Empty)) (TwoNode 7 Empty Empty)
    putStrLn (show (height binTree))
    putStrLn(show (height t7))
    let t8 = add 19 t7
    let t9 = add 19 t8
    putStrLn(show t9)
    let t10 = add 19 t9
    putStrLn(show (height t10))


{- References:
-- Used this link to figure out how to calculate the height of a tree data structure.
[1] https://www.digitalocean.com/community/tutorials/height-of-a-tree-data-structure
-}