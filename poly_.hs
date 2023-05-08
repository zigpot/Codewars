module Polyvariadic where

class SumRes r where 
    polyAdd :: Integer -> r

instance SumRes Integer where
    polyAdd = id

instance (Integral a, SumRes r) => SumRes (a -> r) where
    polyAdd x = polyAdd . (x +) . toInteger

class CatRet a r | r -> a where
    fromLists :: ([a] -> [a]) -> r

instance CatRet a [a] where
    fromLists = ($ [])

instance (CatRet a r) => CatRet a ([a] -> r) where
    fromLists a xs = fromLists (a . (++) xs)

polyList :: (CatRet a r) => r
polyList = fromList id
