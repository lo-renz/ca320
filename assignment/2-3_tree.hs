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


-- Tree from assignment spec can be respresented as follows
myTree1 = ThreeNode 3 10 Empty (TwoNode 6 Empty Empty) (ThreeNode 15 20 Empty (TwoNode 18 Empty Empty) Empty)

twoLeaf x = TwoNode x Empty Empty
threeLeaf x y = ThreeNode x y Empty Empty Empty

myThree2 = ThreeNode 3 10 Empty (twoLeaf 6) (ThreeNode 15 20 Empty (twoLeaf 18) Empty)

addNode :: (Ord t) => t -> TwoThreeTree t -> TwoThreeTree t
-- Adding an element to an empty node.
addNode a Empty = twoLeaf a
addNode x (TwoNode a left right)
    | x < a = TwoNode a (addNode x left) right
    | otherwise = TwoNode a left (addNode x right)

-- Adding an element to a 2-node.
--addNode x y = 
