module Decode where

import Test.QuickCheck.Instances ()
import qualified Data.ByteString.Lazy as BL

-- assuming our encode function is defined
-- encode :: BL.ByteString -> BL.ByteString
import Codec.Binary.Base64 (encode)
-- not a real function available

prop_sizeRatio b =
  BL.length (encode b) ==
      4 * ceiling (fromIntegral (BL.length b) / 3)
