bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "Have a steak!"
    | bmi <= normal = "Supposedly normal!"
    | bmi <= comfortable = "How about a walk?"
    | otherwise = "Skip that snack"
    where bmi = weight / height ^ 2
          (skinny, normal, comfortable) = (18.5, 25.0, 30.0)
