main = do
        putStrLn "What is your name?"
        name <- getLine
        putStrLn ("Hi " ++ name)
