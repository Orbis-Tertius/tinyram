{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Spec.SignedArithmeticSpec (spec) where

import TinyRAM.SignedArithmetic
  ( decodeSignedInt,
    getSign,
    getUnsignedComponent,
    signedMultiplyHigh,
  )
import TinyRAM.Spec.Gen
  ( genSignedInteger,
    genUnsignedInteger,
  )
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
  signedMultiplyHighSpec

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

signedMultiplyHighSpec :: Spec
signedMultiplyHighSpec = describe "signedMultiplyHigh" $
  it "results in the correct output" $
    forAllValid $ \(ws :: WordSize) ->
      forAll (genSignedInteger ws) $ \x ->
        forAll (genSignedInteger ws) $ \y -> do
          let x' = decodeSignedInt ws x
              y' = decodeSignedInt ws y
              z = x' * y'
              zA = abs z
              zL = zA .&. (2 ^ ws - 1)
              zH = zA `shift` negate (fromIntegral ws)
              zS = if z < 0 then -1 else 1
              a = signedMultiplyHigh ws x y
              aS = if unSignedInt a >= 2 ^ (ws - 1) then -1 else 1
              aA = unWord $ unSignedInt a .&. (2 ^ (ws - 1) - 1)
          aS `shouldBe` zS
          aA `shouldBe` zH
          aS * ((aA * (2 ^ ws)) .|. zL) `shouldBe` z

twosComplement :: WordSize -> Sign -> Integer -> SignedInt
twosComplement ws sign u =
  SignedInt . Word $
    case sign of
      -1 -> 2 ^ ws - u
      _ -> u

answer :: Sign -> Integer -> Integer
answer sign u =
  case sign of
    -1 -> negate u
    _ -> u
