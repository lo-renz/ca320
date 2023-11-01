data BinTree t = Empty | Root t (BinTree t) (BinTree t)
               deriving (Eq, Ord, Show)

leaf x = Root x Empty Empty

addNode :: (Ord t) => t -> BinTree t -> BinTree t
addNode a Empty = leaf a
addNode x (Root a left right)
    | x < a = Root a (addNode x left) right
    | otherwise = Root a left (addNode x right)

-- [4,3,1,7,5]
makeTree :: (Ord t) => [t] -> BinTree t
makeTree [] = Empty
makeTree (x:xs) = addNode x (makeTree xs)

-- implementation of preorder traversal
preorder :: BinTree t -> [t]
preorder Empty = []
preorder (Root x left right) = [x] ++ preorder left ++ preorder right

-- implementation of inorder traversal
inorder :: BinTree t -> [t]
inorder Empty = []
inorder (Root x left right) = inorder left ++ [x] ++ inorder right

-- implementation of postorder traversal
postorder :: BinTree t -> [t]
postorder Empty = []
postorder (Root x left right) = postorder left ++ postorder right ++ [x]
