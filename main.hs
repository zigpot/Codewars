import Polyvariadic
import Merge


main = do
    print (polyAdd 1 2 3 4 :: Integer)
    print (polyAdd :: Integer)
    print (polyList [1..4] [102..103] :: [Int])
    print (polyList "It" "Can" "Also" "Concat" "Strings" "!" :: String)
    print (polyList [False, False, True] [True, False, True] :: [Bool])
    print (merge "A" "Merged" "Set" "Of" "Strings" :: String)

