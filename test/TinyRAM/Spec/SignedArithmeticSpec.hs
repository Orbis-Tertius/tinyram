{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.SignedArithmeticSpec ( spec ) where


import TinyRAM.SignedArithmetic (getSign, getUnsignedComponent, decodeSignedInt)
import TinyRAM.Spec.Gen (genUnsignedInteger)
import TinyRAM.Spec.Prelude
import TinyRAM.Types.Sign (Sign)
import TinyRAM.Types.SignedInt (SignedInt (..))
import TinyRAM.Types.UnsignedInt (UnsignedInt (..))
import TinyRAM.Types.Word (Word (..))
import TinyRAM.Types.WordSize (WordSize)


spec :: Spec
spec = describe "SignedArithmetic" $ do
  getSignSpec
  getUnsignedComponentSpec
  decodeSignedIntSpec


getSignSpec :: Spec
getSignSpec = describe "getSign" $
  it "gets the sign of the two's complement representation of an integer" $
    forAllValid $ \(ws :: WordSize) ->
      forAll (genUnsignedInteger (ws - 1)) $ \(u :: Integer) ->
        forAllValid $ \(sign :: Sign) ->
          getSign ws (twosComplement ws sign u)
            `shouldBe` sign


getUnsignedComponentSpec :: Spec
getUnsignedComponentSpec = describe "getUnsignedComponent" $
  it "gets the unsigned component of the two's complement representation of an integer" $
    forAllValid $ \(ws :: WordSize) ->
      forAll (genUnsignedInteger (ws - 1)) $ \(u :: Integer) ->
        forAllValid $ \(sign :: Sign) ->
          getUnsignedComponent ws (twosComplement ws sign u)
            `shouldBe` UnsignedInt (Word u)


decodeSignedIntSpec :: Spec
decodeSignedIntSpec = describe "decodeSignedInt" $
  it "decodes the two's complement representation of an integer" $
    forAllValid $ \(ws :: WordSize) ->
      forAll (genUnsignedInteger (ws - 1)) $ \(u :: Integer) ->
        forAllValid $ \(sign :: Sign) ->
          decodeSignedInt ws (twosComplement ws sign u)
            `shouldBe` answer sign u


twosComplement :: WordSize -> Sign -> Integer -> SignedInt
twosComplement ws sign u =
  SignedInt . Word
  $
  case sign of
    -1 -> 2 ^ ws - u
    _  -> u


answer :: Sign -> Integer -> Integer
answer sign u =
  case sign of
    -1 -> negate u
    _  -> u
