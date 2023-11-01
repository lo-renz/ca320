{- Monkey Puzzle Sort -}
-- A binary search tree has a value at each node. The left subtree of each node hold only
-- values less than at the node, and the right subtree holds only values greater than or
-- equal to that at the node. For example, the following is a binary search tree:
--                                        5
--                                       / \
--                                      1   7
--                                       \
--                                        3
--
-- This can be represented in Haskell by creating a data type for a binary tree.

data BinTree t = Empty | Root t (BinTree t) (BinTree t)
                 deriving (Eq, Ord, Show)

-- The above binary tree would then be represented as:
myTree = Root 5 (Root 1 (Empty) (Root 3 Empty Empty))
                (Root 7 Empty Empty)

-- Since the lead node pattern of Root x Empty Empty will occur often we can define this as
leaf x = Root x Empty Empty

-- and hence the above binary tree could be defined as
myTree1 = Root 5 (Root 1 (Empty) (leaf 3))
                 (leaf 7)

{- a -}
-- Write a recursive function addNode :: (Ord t) => t -> BinTree t -> BinTree t. Which, when given
-- an integer and a binary search tree, will add the integer at the correct position in the tree.
-- In order to insert the integer at the correct position, if the integer is less than the value
-- at the current node, then it is placed in the left subtree, otherwise it is placed in the right
-- subtree. The integer is placed at the first unoccupied node (a leaf). For example, if the integer
-- 4 were added to the above tree, the following tree would result:
--
--                                      5
--                                     / \
--                                    /   \
--                                   1     7
--                                    \
--                                     3
--                                      \
--                                       4

addNode :: (Ord t) => t -> BinTree t -> BinTree t
addNode a Empty = leaf a
addNode x (Root a left right)
    | x < a = Root a (addNode x left) right
    | otherwise = Root a left (addNode x right)

{- b -}
-- Write a recursive function makeTree :: (Ord a) => [a] -> BinTree a. Which, when given a list of
-- integers, will create a binary search tree by inserting the head of the list into the correct in
-- the tree created from the tail of the list. For example, applying makeTree to the list
-- [4,3,1,7,5] would create the tree given above. This function should make use of the addNode function

makeTree :: (Ord t) => [t] -> BinTree t
makeTree [] = Empty
makeTree (x:xs) = addNode x (makeTree xs)

{- c -}
-- The values in a tree can be converted into a list by traversing the tree in a specified order.
-- For example, an inorder traversal traverses the left subtree first, then places the root in the
-- result, and then traverses the right subtree. Write a recurive function inOrder :: BinTree a -> [a]
-- which, when given a tree, will return the list giving the result of an inorder traversal of the tree.
-- For example, applying inorder to the tree above would give the list [1,3,4,5,7]

inOrder :: BinTree a -> [a]
inOrder Empty = []
inOrder (Root x left right) = inOrder left ++ [x] ++ inOrder right

{- d -}
-- Monkey puzzle sort works by creating a binary search tree from a list, and then traversing the list
-- in inorder. Write a function mpSort :: (Ord a) => [a] -> [a] which, when given a list of integers,
-- will return the list in ascending numberical order using monkey puzzle sort. For example:
--     mp sort [4,3,1,7,5] = [1,3,4,5,7]
--
-- This function should make use of the makeTree and inOrder functions.

mpSort :: (Ord a) => [a] -> [a]
mpSort x = inOrder (makeTree x)

{- Higher Order Sort -}

-- Write a higher order function hosort :: (a -> a -> Bool) -> [a] -> [a] which takes two arguments:
-- a relative ordering relation and the list to be sorted. The list should be sorted according to the
-- given ordering relation (all consecutive elements in the list satisfy the relation). For example:
--
-- hosort (>) [4,3,1,7,5] = [7,5,4,3,1]

hoAddNode :: Ord t => (t -> t -> Bool) -> t -> BinTree t -> BinTree t
hoAddNode _ a Empty = leaf a
hoAddNode fn x (Root a left right)
    | fn x a = Root a (hoAddNode fn x left) right
    | otherwise = Root a left (hoAddNode fn x right)

hoMakeTree :: Ord t => (t -> t -> Bool) -> [t] -> BinTree t
hoMakeTree _ [] = Empty
hoMakeTree fn (x:xs) = hoAddNode fn x (hoMakeTree fn xs)

hosort :: Ord t => (t -> t -> Bool) -> [t] -> [t]
hosort fn x = inOrder (hoMakeTree fn x)
