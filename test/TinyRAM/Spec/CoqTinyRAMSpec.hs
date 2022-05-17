module TinyRAM.Spec.CoqTinyRAMSpec
  ( byteToBitString
  , spec
  ) where


import Data.Bits (testBit)
import Data.Word (Word8)

import TinyRAM.Spec.Prelude


spec :: Spec
spec = do
  byteToBitStringSpec


byteToBitStringSpec :: Spec
byteToBitStringSpec =
  describe "byteToBitString" $ do
    it "works as expected on 0" $
      byteToBitString 0 `shouldBe` "00000000"
    it "works as expected on 1" $
      byteToBitString 1 `shouldBe` "00000001"
    it "works as expected on 2" $
      byteToBitString 2 `shouldBe` "00000010"
    it "works as expected on 5" $
      byteToBitString 5 `shouldBe` "00000101"
    it "works as expected on 254" $
      byteToBitString 254 `shouldBe` "11111110"
    it "works as expected on 255" $
      byteToBitString 255 `shouldBe` "11111111"


byteToBitString :: Word8 -> String
byteToBitString w =
  [ if testBit w i then '1' else '0'
  | i <- reverse [0..7]
  ]
