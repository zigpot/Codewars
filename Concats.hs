{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Concats where

class ConcatReturn a r | r -> a where
  fromDifflist :: ([a] -> [a]) -> r

instance ConcatReturn a [a] where
  fromDifflist = ($ [])

instance (ConcatReturn a r) => ConcatReturn a ([a] -> r) where
  fromDifflist a xs = fromDifflist (a . (++) xs)

concats :: (ConcatReturn a r) => r
concats = fromDifflist id

class SumRes r where 
    polyAdd :: Integer -> r

instance SumRes Integer where
    polyAdd = id

instance (Integral a, SumRes r) => SumRes (a -> r) where
    polyAdd x = polyAdd . (x +) . toInteger

-- Examples:
-- concats [1,2,3] [] [4,5] [6] :: [Int]
-- concats "Hello, " "world" "!"
-- fromDifflist reverse "!" "dlrow" " ,olleH"
