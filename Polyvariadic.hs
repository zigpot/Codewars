{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}
{-# language IncoherentInstances #-}

module Polyvariadic where

class (Num n) => SumRes n r where 
    polyAdd :: n -> r

instance (Num n, m~n) => SumRes n m where
    polyAdd x = x

instance (Num n, SumRes n r, n~m) => SumRes n (m->r) where
    polyAdd x = polyAdd . (x +)

class CatRet a r | r -> a where
    fromLists :: ([a] -> [a]) -> r

instance CatRet a [a] where
    fromLists = ($ [])

instance (CatRet a r) => CatRet a ([a] -> r) where
    fromLists a xs = fromLists (a . (++) xs)

polyList :: (CatRet a r) => r
polyList = fromLists id
