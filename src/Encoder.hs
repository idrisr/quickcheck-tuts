module Encoder where

import Test.QuickCheck.Instances ()
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S
import Data.Char (ord)
import Test.QuickCheck

encode :: BL.ByteString -> BL.ByteString
encode = id

prop_sizeRatio :: BL.ByteString -> Bool
prop_sizeRatio b = BL.length (encode b) == 4 * ceiling a
  where
    a :: Double
    a = fromIntegral (BL.length b) / 3

prop_outputAlphabet :: BL.ByteString -> Property
prop_outputAlphabet b =
    h $ used `S.isSubsetOf` allowed
  where
    f, g, h :: Testable prop => prop -> Property
    f = classify (S.size used >= 32) "half-alphabet"
    g = classify (S.size used >= 63) "full-alphabet"
    h = f . g
    used = S.fromList . BL.unpack $ encode b
    allowed =
        S.fromList
            . map (fromIntegral . ord)
            $ ['A' .. 'Z']
            <> ['a' .. 'z']
            <> ['0' .. '9']
            <> ['+', '/', '=']

prop_outputAlphabet2 :: BL.ByteString -> Property
prop_outputAlphabet2 b =
    h $ used `S.isSubsetOf` allowed
  where
    f, g, h, i :: Testable prop => prop -> Property
    f = classify (S.size used >= 32) "half-alphabet"
    g = classify (S.size used >= 63) "full-alphabet"
    i = cover 50 (S.size used >= 63) "full-alphabet"
    h = f . g . i
    used = S.fromList . BL.unpack $ encode b
    allowed =
        S.fromList
            . map (fromIntegral . ord)
            $ ['A' .. 'Z']
            <> ['a' .. 'z']
            <> ['0' .. '9']
            <> ['+', '/', '=']

prop_outputAlphabet3 :: Property
prop_outputAlphabet3 =
    forAll (scale (* 3) (arbitrary :: Gen BL.ByteString)) $ \b ->
        let
            used = S.fromList . BL.unpack $ encode b
            i = cover 50 (S.size used >= 63) "full-alphabet"
            allowed =
                S.fromList
                    . map (fromIntegral . ord)
                    $ ['A' .. 'Z']
                    <> ['a' .. 'z']
                    <> ['0' .. '9']
                    <> ['+', '/', '=']
         in
            i $ used `S.isSubsetOf` allowed
