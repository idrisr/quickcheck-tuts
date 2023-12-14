module FunTest where

import Test.QuickCheck

prop :: Fun String Integer -> Bool
prop (Fun _ f) = f "monkey" == f "banana" || f "banana" == f "elephant"
