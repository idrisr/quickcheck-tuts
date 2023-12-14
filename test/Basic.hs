module Basic where

import Stuff
import Test.Invariant
import Test.QuickCheck hiding (Result, reason)
import Test.QuickCheck.Arbitrary.ADT
import Test.QuickCheck.Property

a :: Gen MyType
a = MyType <$> arbitrary <*> arbitrary <*> arbitrary

b :: Gen MyType2
b = genericArbitrary

myList :: Arbitrary a => Gen [a]
myList =
    oneof
        [ return []
        , (:) <$> arbitrary <*> myList
        ]

myList2 :: Arbitrary a => Gen [a]
myList2 =
    frequency
        [ (1, return [])
        , (4, (:) <$> arbitrary <*> myList2)
        ]

flexList :: Arbitrary a => Gen [a]
flexList = sized $ \n ->
    frequency
        [ (1, return [])
        , (n, (:) <$> arbitrary <*> arbitrary)
        ]

prop_commutativeAdd :: Gen Result
prop_commutativeAdd = do
    (x, y) <- arbitrary :: Gen (Int, Int)
    return $
        if x + y == y + x
            then succeeded{reason = "stupid commutative addition"}
            else failed{reason = "stupid non commutative addition"}

prop_commutativeAdd2 :: Int -> Int -> Bool
prop_commutativeAdd2 x y = x + y == y + x

prop_commutativeAdd3 :: Int -> Int -> Bool
prop_commutativeAdd3 = commutative (+)

prop_associative :: (Num a, Eq a) => a -> a -> a -> Bool
prop_associative = associative (+)

ordered :: Ord b => [b] -> Bool
ordered xs = and (zipWith (<=) xs (drop 1 xs))

insert :: Ord a => a -> [a] -> [a]
insert x xs = takeWhile (< x) xs ++ [x] ++ dropWhile (< x) xs

prop_Insert :: Int -> [Int] -> Property
prop_Insert x xs = ordered xs ==> ordered (insert x xs)
