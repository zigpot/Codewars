{-# LANGUAGE FlexibleInstances #-}
module Merge where

class MergeStrings r where
    merge :: String -> r
instance MergeStrings String where
    merge = id
instance (MergeStrings r) => MergeStrings (String -> r) where
    merge acc = merge .  (++ " ") . (acc ++)
