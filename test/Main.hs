module Main (main) where

import Basic
import Test.QuickCheck

main :: IO ()
main = do
    quickCheck prop_commutativeAdd
    quickCheck prop_commutativeAdd2
    quickCheck prop_commutativeAdd3
    quickCheck (prop_associative :: Float -> Float -> Float -> Bool)
