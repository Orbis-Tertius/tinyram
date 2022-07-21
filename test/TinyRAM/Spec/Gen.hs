{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module TinyRAM.Spec.Gen
  ( genAddress
  , genImmediateOrRegister
  , genInputTape
  , genInstruction
  , genMachineState
  , genParamsMachineState
  , genProgramCounter
  , genRegister
  , genRegisterCount
  , genRegisterValues
  , genSignedInteger
  , genUnsignedInteger
  , genWord
  , genMemoryValues
  , genProgramMemoryValues
  ) where


import           Control.Monad
import           Data.GenValidity.ByteString       ()
import qualified Data.Map                          as Map

import           Data.Tuple.Extra                  (uncurry3)
import           TinyRAM.Bytes                     (bytesPerWord)
import           TinyRAM.Spec.Prelude
import           TinyRAM.Types.Address             (Address (..))
import           TinyRAM.Types.Flag                (Flag)
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (..))
import           TinyRAM.Types.InputTape           (InputTape (..))
import           TinyRAM.Types.Instruction         (Instruction (..))
import           TinyRAM.Types.MachineState        (MachineState (..))
import           TinyRAM.Types.MemoryValues        (MemoryValues (..))
import           TinyRAM.Types.Params              (Params (..))
import           TinyRAM.Types.ProgramCounter      (ProgramCounter (..))
import           TinyRAM.Types.ProgramMemoryValues (ProgramMemoryValues (..))
import           TinyRAM.Types.Register            (Register (..))
import           TinyRAM.Types.RegisterCount       (RegisterCount (..))
import           TinyRAM.Types.RegisterValues      (RegisterValues (..))
import           TinyRAM.Types.Sign                (Sign)
import           TinyRAM.Types.SignedInt           (SignedInt (..))
import           TinyRAM.Types.Word                (Word (..))
import           TinyRAM.Types.WordSize            (WordSize (..))



instance GenValid Flag where
  genValid = elements [0, 1]
  shrinkValid = shrinkValidStructurally


instance GenValid Sign where
  genValid = elements [-1, 1]
  shrinkValid = shrinkValidStructurally


instance GenValid WordSize where
  genValid = oneof [return 8, return 16]
  shrinkValid = shrinkValidStructurally


genAddress :: WordSize -> Gen Address
genAddress ws = Address <$> genWord ws
-- genAddress ws = Address . (`mod` (2 ^ ws - 1)) . (* (fromIntegral $ bytesPerWord ws)) <$> genWord ws


genInputTape :: WordSize -> Gen (InputTape a)
genInputTape ws = InputTape <$> listOf (genWord ws)

-- TODO: SMULH is excluded because it doesn't work well in coq-tinyram. Re-add it.
-- TODO: all jump instructions are excluded because they don't
--   seem to work the same in coq-tinyram and here. Re-add them.
-- TODO: add jumps
genInstruction :: WordSize -> RegisterCount -> Gen Instruction
genInstruction ws rc = oneof $
  ((<$> bop) . uncurry3 <$> [
    And
  , Or
  , Xor
  , Add
  , Sub
  , Mull
  , Umulh
  -- , Smulh
  , Udiv
  , Umod
  , Shl
  , Shr
  ])
  ++
  ((<$> uop) . uncurry <$> [
    Not
  , Mov
  , Cmov
  , Cmpe
  , Cmpa
  , Cmpae
  , Cmpg
  , Cmpge
  ])
  ++
  [
    -- Storeb <$> immOrReg <*> reg
    -- Loadb <$> reg <*> immOrReg
    Storew <$> immOrReg <*> reg
  , Loadw <$> reg <*> immOrReg
  , Read <$> reg <*> immOrReg
  , Answer <$> immOrReg
  ]
  where
    bop = (,,) <$> reg <*> reg <*> immOrReg

    uop = (,) <$> reg <*> immOrReg

    reg = genRegister rc

    immOrReg = genImmediateOrRegister ws rc

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
  <*> genProgramMemoryValues ws rc
  <*> genInputTape ws
  <*> genInputTape ws


genMemoryValues :: WordSize -> Gen MemoryValues
genMemoryValues ws =
  fmap (MemoryValues . Map.fromList
    . zip (Address . Word . (* fromIntegral (bytesPerWord ws)) <$> [0..]))
    . vectorOf ((2 ^ unWordSize ws) `quot` bytesPerWord ws)
    $ genWord ws


genProgramMemoryValues :: WordSize -> RegisterCount -> Gen ProgramMemoryValues
genProgramMemoryValues ws rc =
  fmap (ProgramMemoryValues . Map.fromList . zip addresses) instructions
  where
  addresses = Address . Word . (* (2 * fromIntegral (bytesPerWord ws))) <$> [0..]
  instructions = vectorOf ((2 ^ unWordSize ws) `quot` (2 * bytesPerWord ws)) (genInstruction ws rc)

genParamsMachineState :: Gen (Params, MachineState)
genParamsMachineState = do
  ws <- genValid
  rc <- genRegisterCount ws
  ms <- genMachineState ws rc
  return (Params ws rc, ms)

genProgramCounter :: WordSize -> Gen ProgramCounter
genProgramCounter ws = ((.&. (2 ^ ws - 1)) . (* (fromIntegral $ bytesPerWord ws))) . ProgramCounter <$> genAddress ws


genRegisterCount :: WordSize -> Gen RegisterCount
genRegisterCount (WordSize 16) = RegisterCount <$> choose (2, 32)
genRegisterCount (WordSize 8)  = return (RegisterCount 2)
genRegisterCount _             = error "genRegisterCount: unsupported word size"
  -- RegisterCount <$> choose (2, min 32 (2 ^ ((ws - 6) `quot` 2)))


genRegisterValues :: WordSize -> RegisterCount -> Gen RegisterValues
genRegisterValues ws (RegisterCount n) =
  RegisterValues . Map.fromList
  <$>
  zipWithM
      (\a b -> (a,) <$> b)
      (Register <$> [0..n-1])
      (repeat (genWord ws))


genSignedInteger :: WordSize -> Gen SignedInt
genSignedInteger ws = SignedInt <$> genWord ws


genUnsignedInteger :: WordSize -> Gen Integer
genUnsignedInteger (WordSize ws) = choose (0, 2 ^ ws - 1)


genWord :: WordSize -> Gen Word
genWord ws = Word <$> genUnsignedInteger ws
