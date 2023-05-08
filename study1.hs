divide :: Int -> Int -> Maybe Int
divide _ 0 = Nothing
divide x y = Just $ div x y
