mysteryFn :: String -> String
mysteryFn x = [y | y <- x, y `elem` ['a'..'z']]
