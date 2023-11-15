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
                    | OneRoot t (TwoThreeTree t) (TwoThreeTree t)
                    | TwoRoot t t (TwoThreeTree t) (TwoThreeTree t) (TwoThreeTree t)
                    deriving (Eq, Ord, Show)


twoLeaf x = OneRoot x Empty Empty
threeLeaf x y = TwoRoot Empty Empty Empty

addNode :: (Ord t) => t -> TwoThreeTree t -> TwoThreeTree t
-- Adding an element to an empty node.
addNode a Empty = twoLeaf a
addNode x (Root a left right)
    | x < a = OneRoot a (addNode x left) right
    | otherwise = OneRoot a left (addNode x right)

-- Adding an element to a 2-node.
addNode x y = 
