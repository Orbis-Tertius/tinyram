{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.DecodeInstruction
  ( decodeInstruction
  , bitsPerRegister
  ) where


import           TinyRAM.Prelude
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (..))
import           TinyRAM.Types.Instruction         (Instruction (..))
import           TinyRAM.Types.Opcode              (Opcode (..))
import           TinyRAM.Types.Register            (Register (..))
import           TinyRAM.Types.RegisterCount       (RegisterCount (..))
import           TinyRAM.Types.Word                (Word (..))
import           TinyRAM.Types.WordSize            (WordSize)


decodeInstruction :: WordSize -> RegisterCount -> (Word, Word) -> Instruction
decodeInstruction ws rc i@(_, _i1) =
  Instruction
  (decodeOpcode ws _i1)
  (decodeA ws i)
  (decodeRI ws rc _i1)
  (decodeRJ ws rc _i1)


decodeOpcode :: WordSize -> Word -> Opcode
decodeOpcode ws i1 = Opcode (fromIntegral opcode)
  where
    opcode :: Integer
    opcode = fromIntegral i1 `shift` (- fromIntegral ws + 5)


decodeRI :: WordSize -> RegisterCount -> Word -> Register
decodeRI ws rc (Word w) = Register . fromIntegral
  $ shifted .&. registerBitmask rc
  where
    shifted :: Integer
    shifted = w `shift` (- fromIntegral ws + bitsPerRegister rc + 6)


decodeRJ :: WordSize -> RegisterCount -> Word -> Register
decodeRJ ws rc (Word w) = Register . fromIntegral
  $ shifted .&. registerBitmask rc
  where
    shifted :: Integer
    shifted = w `shift` (- fromIntegral ws + 2 * bitsPerRegister rc + 6)


bitsPerRegister :: RegisterCount -> Int
bitsPerRegister (RegisterCount rc) = ceiling (logBase 2 (fromIntegral rc) :: Double)


registerBitmask :: RegisterCount -> Integer
registerBitmask rc = 2 ^ bitsPerRegister rc - 1


decodeA :: WordSize -> (Word, Word) -> ImmediateOrRegister
decodeA ws (i0, i1) =
  if flagBitmask then IsImmediate i0 else IsRegister . Register . fromIntegral $ i0
  where
    flagBitmask :: Bool
    flagBitmask = shifted .&. 0x01 == 1

    shifted :: Integer
    shifted = fromIntegral i1 `shift` (- fromIntegral ws + 6)
