{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module TinyRAM.Spec.Gen
  ( genAddress
  , genImmediateOrRegister
  , genInputTape
  , genInstruction
  , genMachineState
  , genMemoryValues
  , genParamsMachineState
  , genProgramCounter
  , genRegister
  , genRegisterCount
  , genRegisterValues
  , genSignedInteger
  , genUnsignedInteger
  , genWord
  ) where


import Data.GenValidity.ByteString ()
import qualified Data.Map as Map

import TinyRAM.Spec.Prelude
import TinyRAM.Types.Address (Address (..))
import TinyRAM.Types.Flag (Flag)
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (..))
import TinyRAM.Types.InputTape (InputTape (..))
import TinyRAM.Types.Instruction (Instruction (..))
import TinyRAM.Types.MachineState (MachineState (..))
import TinyRAM.Types.MemoryValues (MemoryValues (..))
import TinyRAM.Types.Opcode (Opcode (..))
import TinyRAM.Types.Params (Params (..))
import TinyRAM.Types.ProgramCounter (ProgramCounter (..))
import TinyRAM.Types.Register (Register (..))
import TinyRAM.Types.RegisterCount (RegisterCount (..))
import TinyRAM.Types.RegisterValues (RegisterValues (..))
import TinyRAM.Types.Sign (Sign)
import TinyRAM.Types.SignedInt (SignedInt (..))
import TinyRAM.Types.WordSize (WordSize (..))
import TinyRAM.Types.Word (Word (..))


instance GenValid Opcode where
  genValid = Opcode <$> oneof [choose (0, 22), choose (28, 30)]
  shrinkValid = shrinkValidStructurally


instance GenValid Flag where
  genValid = elements [0, 1]
  shrinkValid = shrinkValidStructurally


instance GenValid Sign where
  genValid = elements [-1, 1]
  shrinkValid = shrinkValidStructurally


instance GenValid WordSize where
  genValid = WordSize . (8*) <$> choose (2, 16)
  shrinkValid = shrinkValidStructurally


genAddress :: WordSize -> Gen Address
genAddress ws = Address <$> genWord ws


genInputTape :: WordSize -> Gen (InputTape a)
genInputTape ws = InputTape <$> listOf (genWord ws)


genInstruction :: WordSize -> RegisterCount -> Gen Instruction
genInstruction ws rc =
  Instruction
  <$> genValid
  <*> genImmediateOrRegister ws rc
  <*> genRegister rc
  <*> genRegister rc


genRegister :: RegisterCount -> Gen Register
genRegister (RegisterCount rc) = Register <$> choose (0, rc - 1)


genImmediateOrRegister :: WordSize -> RegisterCount -> Gen ImmediateOrRegister
genImmediateOrRegister ws rc =
  oneof [IsImmediate <$> genWord ws, IsRegister <$> genRegister rc]


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


genParamsMachineState :: Gen (Params, MachineState)
genParamsMachineState = do
  ws <- genValid
  rc <- genRegisterCount ws
  ms <- genMachineState ws rc
  return (Params ws rc, ms)


genProgramCounter :: WordSize -> Gen ProgramCounter
genProgramCounter ws = ProgramCounter <$> genAddress ws


genRegisterCount :: WordSize -> Gen RegisterCount
genRegisterCount (WordSize ws) =
  RegisterCount <$> choose (2, min 256 (2 ^ ((ws - 6) `quot` 2)))


genRegisterValues :: WordSize -> RegisterCount -> Gen RegisterValues
genRegisterValues ws (RegisterCount n) =
  RegisterValues . Map.fromList
  <$>
  sequence
    (zipWith
      (\a b -> (a,) <$> b)
      (Register <$> [0..n-1])
      (repeat (genWord ws)))


genSignedInteger :: WordSize -> Gen SignedInt
genSignedInteger ws = SignedInt <$> genWord ws


genUnsignedInteger :: WordSize -> Gen Integer
genUnsignedInteger (WordSize ws) = choose (0, 2 ^ ws - 1)


genWord :: WordSize -> Gen Word
genWord ws = Word <$> genUnsignedInteger ws
