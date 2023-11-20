data Tree a = EmptyTree
            | Node a (Tree a) (Tree a) deriving (Read, Eq)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x <  a = Node a (treeInsert x left) right
    | x >  a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x <  a = treeElem x left
    | x >  a = treeElem x right

treeHeight :: Tree a -> Int
treeHeight EmptyTree = 0
treeHeight (Node a left right) = maximum [1, lh, rh]
    where lh = 1 + treeHeight left
          rh = 1 + treeHeight right

instance (Show a) => Show (Tree a) where
    show EmptyTree = ""
    show tree = show' tree 0 (widestElement tree + 1)

show' :: (Show a) => Tree a -> Int -> Int -> String
show' EmptyTree _ _ = " "
show' (Node a left right) depth width =
    leftside ++ "\n" ++ center ++ rightside
    where center    = replicate depth ' ' ++ show a
          leftside  = show' left (depth + width) width
          rightside = show' right (depth + width) width

widestElement :: (Show a) => Tree a -> Int
widestElement EmptyTree = 0
widestElement (Node center left right) = maximum [l, r, c]
    where l = widestElement left
          r = widestElement right
          c = length $ show center

makeTree :: (Ord a) => [a] -> Tree a
makeTree = foldr treeInsert EmptyTree . reverse