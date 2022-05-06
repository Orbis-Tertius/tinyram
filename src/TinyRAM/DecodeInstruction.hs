{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.DecodeInstruction
  ( decodeInstruction
  , bitsPerRegister
  ) where


import Data.Bits (rotate)

import           TinyRAM.Prelude
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (..))
import           TinyRAM.Types.Instruction         (Instruction (..))
import           TinyRAM.Types.Opcode              (Opcode (..))
import           TinyRAM.Types.Register            (Register (..))
import           TinyRAM.Types.RegisterCount       (RegisterCount (..))
import           TinyRAM.Types.Word                (Word (..))
import           TinyRAM.Types.WordSize            (WordSize)


decodeInstruction :: WordSize -> RegisterCount -> (Word, Word) -> Instruction
decodeInstruction ws rc i@(i0, _i1) =
  Instruction
  (decodeOpcode ws i0)
  (decodeA ws i)
  (decodeRI ws rc i0)
  (decodeRJ ws rc i0)


decodeOpcode :: WordSize -> Word -> Opcode
decodeOpcode ws i1 = Opcode (fromIntegral opcode)
  where
    opcode :: Integer
    opcode = (fromIntegral i1 `rotate` (- fromIntegral ws + 5))
             .&. opcodeBitmask

    opcodeBitmask :: Integer
    opcodeBitmask = 31


decodeRI :: WordSize -> RegisterCount -> Word -> Register
decodeRI ws rc (Word w) = Register . fromIntegral
  $ shifted .&. registerBitmask rc
  where
    shifted :: Integer
    shifted = w `rotate` (- fromIntegral ws + bitsPerRegister rc + 6)


decodeRJ :: WordSize -> RegisterCount -> Word -> Register
decodeRJ ws rc (Word w) = Register . fromIntegral
  $ shifted .&. registerBitmask rc
  where
    shifted :: Integer
    shifted = w `rotate` (- fromIntegral ws + 2 * bitsPerRegister rc + 6)


bitsPerRegister :: RegisterCount -> Int
bitsPerRegister (RegisterCount rc) = ceiling (logBase 2 (fromIntegral rc) :: Double)


registerBitmask :: RegisterCount -> Integer
registerBitmask rc = 2 ^ bitsPerRegister rc - 1


decodeA :: WordSize -> (Word, Word) -> ImmediateOrRegister
decodeA ws (i0, i1) =
  if flagBitmask then IsImmediate i1 else IsRegister . Register . fromIntegral $ i1
  where
    flagBitmask :: Bool
    flagBitmask = shifted .&. 1 == 1

    shifted :: Integer
    shifted = fromIntegral i0 `rotate` (- fromIntegral ws + 6)
