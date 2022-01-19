{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module TinyRAM.Spec.Gen
  ( genAddress
  , genInputTape
  , genMachineState
  , genMemoryValues
  , genProgramCounter
  , genRegisterValues
  , genUnsignedInteger
  , genWord
  ) where


import Data.GenValidity.ByteString ()
import qualified Data.Map as Map

import TinyRAM.Spec.Prelude
import TinyRAM.Types.Address (Address (..))
import TinyRAM.Types.Flag (Flag)
import TinyRAM.Types.InputTape (InputTape (..))
import TinyRAM.Types.MachineState (MachineState (..))
import TinyRAM.Types.MemoryValues (MemoryValues (..))
import TinyRAM.Types.ProgramCounter (ProgramCounter (..))
import TinyRAM.Types.Register (Register (..))
import TinyRAM.Types.RegisterCount (RegisterCount (..))
import TinyRAM.Types.RegisterValues (RegisterValues (..))
import TinyRAM.Types.Sign (Sign)
import TinyRAM.Types.WordSize (WordSize (..))
import TinyRAM.Types.Word (Word (..))


instance GenValid Flag where
  genValid = elements [0, 1]
  shrinkValid = shrinkValidStructurally


instance GenValid Sign where
  genValid = elements [-1, 1]
  shrinkValid = shrinkValidStructurally


instance GenValid WordSize where
  genValid = WordSize . (8*) <$> choose (1, 32)
  shrinkValid = shrinkValidStructurally


genAddress :: WordSize -> Gen Address
genAddress ws = Address <$> genWord ws


genInputTape :: WordSize -> Gen (InputTape a)
genInputTape ws = InputTape <$> listOf (genWord ws)


genMachineState :: WordSize -> RegisterCount -> Gen MachineState
genMachineState ws rc =
  MachineState
  <$> genProgramCounter ws
  <*> genRegisterValues ws rc
  <*> genValid
  <*> genMemoryValues ws
  <*> genInputTape ws
  <*> genInputTape ws


genMemoryValues :: WordSize -> Gen MemoryValues
genMemoryValues ws =
  fmap (MemoryValues . Map.fromList) . listOf
    $ (,) <$> genAddress ws <*> genWord ws


genProgramCounter :: WordSize -> Gen ProgramCounter
genProgramCounter ws = ProgramCounter <$> genAddress ws


genRegisterValues :: WordSize -> RegisterCount -> Gen RegisterValues
genRegisterValues ws (RegisterCount n) =
  RegisterValues
  <$>
  sequence
  (Map.fromList
    (zip (Register <$> [0..n-1])
         (repeat (genWord ws))))


genUnsignedInteger :: WordSize -> Gen Integer
genUnsignedInteger (WordSize ws) = choose (0, 2 ^ ws - 1)


genWord :: WordSize -> Gen Word
genWord (WordSize ws) = Word <$> choose (0, 2 ^ ws)
