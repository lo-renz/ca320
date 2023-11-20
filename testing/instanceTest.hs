-- Define a custom data type
data Person = Person { firstName :: String, lastName :: String, age :: Int }

-- Define a Show instance for the Person type
instance Show Person where
    show (Person first last age) = "Person { firstName = " ++ show first ++ ", lastName = " ++ show last ++ ", age = " ++ show age ++ " }"

-- Example usage
main :: IO ()
main = do
    let person = Person "John" "Doe" 25
    putStrLn $ show person
