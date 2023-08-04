{-# LANGUAGE DeriveGeneric #-}

module Stuff where

import GHC.Generics
-- import Generic.Random.Generic

data MyType = MyType {
    foo :: Int
    , bar :: Bool
    , baz :: Float
} deriving Show

data MyType2 = MyType2 {
    foo2 :: Int
    , bar2 :: Bool
    , baz2 :: Float
} deriving (Show, Generic)
