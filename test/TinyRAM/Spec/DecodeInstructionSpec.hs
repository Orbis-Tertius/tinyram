{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.DecodeInstructionSpec ( spec ) where


import           TinyRAM.DecodeInstruction   (decodeInstruction)
import           TinyRAM.EncodeInstruction   (encodeInstruction)
import           TinyRAM.Spec.Gen            (genInstruction, genRegisterCount)
import           TinyRAM.Spec.Prelude
import           TinyRAM.Types.Instruction   (Instruction)
import           TinyRAM.Types.RegisterCount (RegisterCount (..))
import           TinyRAM.Types.WordSize      (WordSize (..))

spec :: Spec
spec = describe "decodeInstruction" $
  it "passes round trip tests" $
    forAllValid $ \(ws :: WordSize) ->
      forAll (genRegisterCount ws) $ \(rc :: RegisterCount) ->
        forAll (genInstruction ws rc) $ \(i :: Instruction) ->
          let encW = encodeInstruction i ws rc
           in decodeInstruction rc (encW, encW `div` (2 ^ ws))
              `shouldBe` i
