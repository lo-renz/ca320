data TwoThreeTree t
  = Empty
  | TwoNode t (TwoThreeTree t) (TwoThreeTree t)
  | ThreeNode t t (TwoThreeTree t) (TwoThreeTree t) (TwoThreeTree t)
  deriving (Eq, Ord, Show)

indent :: [String] -> [String]
indent = map ("        "++)

layoutTree :: (Show a) => TwoThreeTree a -> [String]
layoutTree Empty = ["nil"]
layoutTree (TwoNode x left right) = indent (layoutTree left) ++ [show x] ++ indent (layoutTree right)
layoutTree (ThreeNode x y left middle right) = indent (layoutTree left) ++ [show (x, y)] ++ (layoutTree middle) ++ indent (layoutTree right)

prettyPrint :: (Show a) => TwoThreeTree a -> String
prettyPrint = unlines . layoutTree

main :: IO()
main = do
  -- 2-node tree
  let twoNodeTree = TwoNode 5 (TwoNode 1 Empty (TwoNode 3 Empty Empty)) (TwoNode 7 Empty Empty)

  -- 3-node tree
  let tree = ThreeNode 3 10 Empty (TwoNode 6 Empty Empty) (ThreeNode 15 20 Empty (TwoNode 18 Empty Empty) Empty)

  -- print (layoutTree twoNodeTree)
  print (layoutTree tree)
  putStr (prettyPrint tree)

