{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.DecodeInstructionSpec ( spec ) where


import TinyRAM.DecodeInstruction (decodeInstruction, bitsPerRegister)
import TinyRAM.Spec.Gen (genInstruction, genRegisterCount)
import TinyRAM.Spec.Prelude
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (..))
import TinyRAM.Types.Instruction (Instruction)
import TinyRAM.Types.RegisterCount (RegisterCount (..))
import TinyRAM.Types.Word (Word (..))
import TinyRAM.Types.WordSize (WordSize (..))


spec :: Spec
spec = describe "decodeInstruction" $
  it "passes round trip tests" $
    forAllValid $ \(ws :: WordSize) ->
      forAll (genRegisterCount ws) $ \(rc :: RegisterCount) ->
        forAll (genInstruction ws rc) $ \(i :: Instruction) ->
          decodeInstruction rc (encodeInstruction rc i)
            `shouldBe` i


encodeInstruction :: RegisterCount -> Instruction -> (Word, Word)
encodeInstruction rc i =
  ( Word $ fromIntegral (i ^. #opcode . #unOpcode)
       .|. aBit
       .|. fromIntegral (i ^. #ri . #unRegister) `shift` 6
       .|. fromIntegral (i ^. #rj . #unRegister) `shift` (6 + bitsPerRegister rc)
  , aVal
  )
  where
    aBit =
      case i ^. #a of
        IsImmediate _ -> 2 ^ (5 :: Integer)
        IsRegister  _ -> 0
    aVal =
      case i ^. #a of
        IsImmediate w -> w
        IsRegister r -> fromIntegral r
