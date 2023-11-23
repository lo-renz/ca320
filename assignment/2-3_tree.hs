import Data.Char

{-
 A datatype which depicts that a 2-3 Tree can either be:
 - An empty tree

 - A node with 1 root element and two subtrees.

 - A node with 2 root elements and three subtrees.
-}
data TwoThreeTree t
  = Empty
  | TwoNode t (TwoThreeTree t) (TwoThreeTree t)
  | ThreeNode t t (TwoThreeTree t) (TwoThreeTree t) (TwoThreeTree t)
  deriving (Eq, Ord, Show)

-- Using the above datatype the tree from assignment spec can be respresented as follows:
-- myTree1 = ThreeNode 3 10 Empty (TwoNode 6 Empty Empty) (ThreeNode 15 20 Empty (TwoNode 18 Empty Empty) Empty)

twoLeaf x = TwoNode x Empty Empty

threeLeaf x y = ThreeNode x y Empty Empty Empty

-- Using the above, we can rewrite the tree as:
-- myThree2 = ThreeNode 3 10 Empty (twoLeaf 6) (ThreeNode 15 20 Empty (twoLeaf 18) Empty)

-- Implementation of add(X, T), which returns the 2-3 Tree from adding X to the 2-3 Tree T.
{- Explanation:
  A function which adds a value to a specified tree. It uses pattern matching to check whether the tree is
  empty, a 2-node or 3-node tree and acts accordingly.

  If an element is being added to an empty tree, then the returned tree is a 2-node tree which contains the
  added element and two empty subtrees.

  For elements added to a 2-node tree, depending on the element being added to the 2-node tree, the smaller
  element is added to the left while the bigger element is added to the right.

  I use recursion to add elements to a 3-node tree. This is done depending on the element being added. Let's
  define x to be the element added to the tree t. Let's also define y and z to be the elements found in the
  tree t in respective order. If x <= y then the element is added to the left subtree. If y < x <= z then the
-}
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

-- Implementation of member(X, T), which returns true if X is in the 2-3 Tree T.
{- Explanation:

 A function which returns True or False based on whether a value X is in the tree T. It uses pattern matching to check whether the tree is
 empty, a 2-node or 3-node tree and acts accordingly.

 My base case for the 2-node trees returns True is the value x is equal to the value of the root. If not then it will recursively check the
 left and right subtrees and make the same check. If none of the patterns match then False is returned.

 I applied the same logic for the 3-node trees. If the value x is equal to either the roots y or z then return True. Else recursively check
 either the left, middle and right subtrees. If none of the patterns match then False is returned.
-}
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
--prettyPrint :: (Eq t, Show t) => TwoThreeTree t -> IO ()
--prettyPrint (TwoNode x left right)

-- A main function which shows off the implementation of the functions by printing the output of the functions to standard output.
main :: IO ()
main = do
  let xList = [3, 10, 15, 20, 6, 18]

  -- To show off the implementation of add(X, T).
  -- Which returns the 2-3 Tree from adding X to the 2-3 Tree T.
  -- print () = putStrLn (show ())
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
  putStrLn (show (height emptyTree))
  let twoNodeTree = TwoNode 5 (TwoNode 1 Empty (TwoNode 3 Empty Empty)) (TwoNode 7 Empty Empty)
  putStrLn (show (height twoNodeTree))
  putStrLn (show (height t7))
  let t8 = add 19 t7
  let t9 = add 19 t8
  putStrLn (show t9)
  let t10 = add 19 t9
  putStrLn (show (height t10))

  print "----" -- Just to separate the function prints.

 -- To show off the implementation of prettyPrint(T).
 -- Which displays the contetns of the 2-3 Tree in an easily readable format.
  --let twoNodeTree = TwoNode 5 (TwoNode 1 Empty (TwoNode 3 Empty Empty)) (TwoNode 7 Empty Empty)
  --prettyPrint(twoNodeTree)

{- References:
-- Used this link to figure out how to calculate the height of a tree data structure.
[1] https://www.digitalocean.com/community/tutorials/height-of-a-tree-data-structure
fthree-}