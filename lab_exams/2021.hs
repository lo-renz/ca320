{- Q1 -}
-- Write an expression that evaluates to a list of the odd numbers that are a multiple of 3 and less than 100.
oddMultiplesOf3 :: [Int]
oddMultiplesOf3 = [x | x <- [1..100], x `mod` 2 == 0, x `mod` 3 == 0]

{- Q2 -}
-- Write a function that takes two integers and evaluates to their sum if either integer is odd, otherwise evaluates to their difference.
sumOrDiff :: Int -> Int -> Int
sumOrDiff x y
    | odd x || odd y = x + y
    | otherwise = x - y

{- Q3 -}
-- Write a function, join, that takes two lists and returns a list that is the concatenation of both lists except the elements in the
-- first list that are already in the second list are not added to the resulting list, e.g. join [1,2,3] [2,4,6,8] evaluates to
-- [1,3,2,4,6,8]
join :: (Ord a) => [a] -> [a] -> [a]
join [] ys = ys
join (x:xs) ys
    | x `elem` ys = join xs ys
    | otherwise = x:(join xs ys)

{- Q4 -}
-- Write a data type for a binary tree.

data BinTree t = Empty | Root t (BinTree t) (BinTree t)
                 deriving (Eq, Ord, Show)

leaf x = Root x Empty Empty

{- Q5 -}
-- Write a function that converts a binary tree into a list by doing a preorder traversal of the binary tree. Remember a preorder traversal
-- visits the parent node, then the left child and then the right child.
addNode :: (Ord t) => t -> BinTree t -> BinTree t
addNode a Empty = leaf a
addNode x (Root a left right)
    | x < a = Root a (addNode x left) right
    | otherwise = Root a left (addNode x right)

-- makeTree [4,3,1,5,7]
makeTree :: (Ord t) => [t] -> BinTree t
makeTree [] = Empty
makeTree (x:xs) = addNode x (makeTree xs)

preorder :: BinTree t -> [t]
preorder Empty = []
preorder (Root x left right) = [x] ++ preorder left ++ preorder right

-- Testing the post order traversal
postorder :: BinTree t -> [t]
postorder Empty = []
postorder (Root x left right) = postorder left ++ postorder right ++ [x]

-- Testing the inorder traversal
inorder :: BinTree t -> [t]
inorder Empty = []
inorder (Root x left right) = inorder left ++ [x] ++ inorder right
