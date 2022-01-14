{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.DecodeInstruction ( decodeInstruction ) where


import TinyRAM.Prelude
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (..))
import TinyRAM.Types.Instruction (Instruction (..))
import TinyRAM.Types.Opcode (Opcode (..))
import TinyRAM.Types.Register (Register (..))
import TinyRAM.Types.RegisterCount (RegisterCount (..))
import TinyRAM.Types.Word (Word)


decodeInstruction :: RegisterCount -> (Word, Word) -> Instruction
decodeInstruction rc i@(i0, _i1) =
  Instruction
  (decodeOpcode i0)
  (decodeA i)
  (decodeRI rc i0)
  (decodeRJ rc i0)


decodeOpcode :: Word -> Opcode
decodeOpcode i0 = Opcode . fromIntegral $ i0 .&. opcodeBitmask


opcodeBitmask :: Word
opcodeBitmask = 31


decodeRI :: RegisterCount -> Word -> Register
decodeRI = todo


decodeRJ :: RegisterCount -> Word -> Register
decodeRJ = todo


decodeA :: (Word, Word) -> ImmediateOrRegister
decodeA = todo


todo :: a
todo = todo
