module Main (main) where

import Basic
import Test.QuickCheck

main :: IO ()
main = do
    _ <- quickCheck prop_commutativeAdd
    _ <- quickCheck prop_commutativeAdd2
    _ <- quickCheck prop_commutativeAdd3
    quickCheck (prop_associative :: Float -> Float -> Float -> Bool)
