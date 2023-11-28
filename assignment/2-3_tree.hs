data TwoThreeTree t
  = Empty
  | TwoNode t (TwoThreeTree t) (TwoThreeTree t)
  | ThreeNode t t (TwoThreeTree t) (TwoThreeTree t) (TwoThreeTree t)
  deriving (Eq, Ord, Show)

twoLeaf x = TwoNode x Empty Empty

add :: (Ord t) => t -> TwoThreeTree t -> TwoThreeTree t
add x Empty = twoLeaf x
add x (TwoNode y left right)
  | x < y = ThreeNode x y left Empty right
  | otherwise = ThreeNode y x left Empty right
add x (ThreeNode y z left middle right)
  | x <= y = ThreeNode y z (add x left) middle right
  | x > y && x <= z = ThreeNode y z left (add x middle) right
  | x > z = ThreeNode y z left middle (add x right)
  | otherwise = error "Something wrong happened"

member :: (Ord t) => t -> TwoThreeTree t -> Bool
member x Empty = False
member x (TwoNode y left right)
  | x == y = True
  | x < y = member x left
  | x > y = member x right
  | otherwise = False
member x (ThreeNode y z left middle right)
  | x == y = True
  | x == z = True
  | x <= y = member x left
  | x > y && x <= z = member x middle
  | x > z = member x right
  | otherwise = False

-- Implementation of height(T), which returns the height of T.
{- Explanation: [1]
 A function which calulates the height of a tree T. It uses pattern matching to check whether the tree
 is empty, a 2-node or 3-node tree and acts accordingly.

 The height of an empty tree is -1. [1]

 To calculate the height of a binary tree I found the height of its left subtree and right subtree recursively and added 1 to them
 Once I found the height of each subtree, I compared them and took the greatest height of the subtrees by using the built-in funtion 'max'.
-}
height :: TwoThreeTree t -> Int
height Empty = -1
height (TwoNode x left right) = (max (height left) (height right)) + 1
height (ThreeNode x y left middle right) = (max (height left) (max (height middle) (height right))) + 1

-- Implementation of prettyPrint(T), which is always true and displays the 2- Tree T.
{- Explanation:
-}

-- Space between the branches
-- [2]
indent :: [String] -> [String]
indent = map ("        "++)

layoutTree :: (Show a) => TwoThreeTree a -> [String]
layoutTree Empty = ["nil"]
layoutTree (TwoNode x left right) = indent (layoutTree left) ++ [show x] ++ indent (layoutTree right)
layoutTree (ThreeNode x y left middle right) = indent (layoutTree left) ++ [show (x, y)] ++  indent (layoutTree middle) ++ indent (layoutTree right)
-- layoutTree (ThreeNode x y left middle right) = indent (layoutTree left) ++ [show (x, y)] ++ indent (layoutTree middle) ++ indent (layoutTree right)

prettyPrint :: (Show a) => TwoThreeTree a -> String
prettyPrint = unlines.layoutTree

-- A main function which shows off the implementation of the functions by printing the output of the functions to standard output.
main :: IO ()
main = do
  let xList = [3, 10, 15, 20, 6, 18]

  -- To show off the implementation of add(X, T).
  -- Which returns the 2-3 Tree from adding X to the 2-3 Tree T.
  -- print () = putStrLn (show ())
  let t1 = Empty
  print (add (head xList) t1)
  let t2 = add (head xList) t1
  print (add (xList !! 1) t2)
  let t3 = add (xList !! 1) t2
  print (add (xList !! 2) t3)
  let t4 = add (xList !! 2) t3
  print (add (xList !! 3) t4)
  let t5 = add (xList !! 3) t4
  print (add (xList !! 4) t5)
  let t6 = add (xList !! 4) t5
  print (add (xList !! 5) t6)

  print "----" -- Just to separate the function prints.

  -- To show off the implementation of member(X, T).
  -- Which returns true if X is in the 2-3 Tree T.
  let t7 = add (xList !! 5) t6
  print (member 3 t7)
  print (member 10 t7)
  print (member 15 t7)
  print (member 20 t7)
  print (member 6 t7)
  print (member 100 t7)
  print (member 18 t7)
  print (member 0 t7)

  print "----" -- Just to separate the function prints.

  -- To show off the implementation of height(T).
  -- Which returns the height of the 2-3 Tree.
  let emptyTree = Empty
  print (height emptyTree)
  let twoNodeTree = TwoNode 5 (TwoNode 1 Empty (TwoNode 3 Empty Empty)) (TwoNode 7 Empty Empty)
  print (height twoNodeTree)
  print (height t7)
  let t8 = add 19 t7
  let t9 = add 19 t8
  print t9
  let t10 = add 19 t9
  print (height t7)

  print "----" -- Just to separate the function prints.

 -- To show off the implementation of prettyPrint(T).
 -- Which displays the contetns of the 2-3 Tree in an easily readable format.
  let twoNodeTree = TwoNode 5 (TwoNode 1 Empty (TwoNode 3 Empty Empty)) (TwoNode 7 Empty Empty)
  print (layoutTree twoNodeTree)

  print "----" -- Just to separate the function prints.
  -- Testing the print for TwoNode trees.
  putStrLn (prettyPrint twoNodeTree)

  print "----" -- Just to separate the function prints.

  -- Testing the prettyPrint for ThreeNode trees.
  -- print (layoutTree t7)
  putStrLn (prettyPrint t7)

{- References:
-- Used this link to figure out how to calculate the height of a tree data structure.
[1] https://www.digitalocean.com/community/tutorials/height-of-a-tree-data-structurefthree
[2] https://stackoverflow.com/questions/19082560/haskell-pretty-print-binary-tree-not-displaying-properly/19083798#19083798?newreg=75adfcd525aa4e24b6ed7c8365112c76
-}
