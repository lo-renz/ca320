bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "Have a steak!"
    | weight / height ^ 2 <= 25.0 = "Supposedly normal!"
    | weight / height ^ 2 <= 30.0 = "How about a walk?"
    | otherwise            = "Skip that snack"
