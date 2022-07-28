{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}

module TinyRAM.EncodeInstruction (encodeInstruction) where

import           TinyRAM.Prelude
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (..))
import           TinyRAM.Types.Instruction         (Instruction (..))
import           TinyRAM.Types.Word                (Word (..))
import           TinyRAM.Types.WordSize            (WordSize (..))

import           TinyRAM.DecodeInstruction         (bitsPerRegister)
import           TinyRAM.Types.Register
import           TinyRAM.Types.RegisterCount

encodeInstruction :: WordSize -> RegisterCount -> Instruction -> (Word, Word)
encodeInstruction w k instr = case instr of
  And    ri rj a -> encode (0, ri, rj, a)
  Or     ri rj a -> encode (1, ri, rj, a)
  Xor    ri rj a -> encode (2, ri, rj, a)
  Not    ri    a -> encode (3, ri, Register 0, a)
  Add    ri rj a -> encode (4, ri, rj, a)
  Sub    ri rj a -> encode (5, ri, rj, a)
  Mull   ri rj a -> encode (6, ri, rj, a)
  Umulh  ri rj a -> encode (7, ri, rj, a)
  Smulh  ri rj a -> encode (8, ri, rj, a)
  Udiv   ri rj a -> encode (9, ri, rj, a)
  Umod   ri rj a -> encode (10, ri, rj, a)
  Shl    ri rj a -> encode (11, ri, rj, a)
  Shr    ri rj a -> encode (12, ri, rj, a)
  Cmpe   ri    a -> encode (13, ri, Register 0, a)
  Cmpa   ri    a -> encode (14, ri, Register 0, a)
  Cmpae  ri    a -> encode (15, ri, Register 0, a)
  Cmpg   ri    a -> encode (16, ri, Register 0, a)
  Cmpge  ri    a -> encode (17, ri, Register 0, a)
  Mov    ri    a -> encode (18, ri, Register 0, a)
  Cmov   ri    a -> encode (19, ri, Register 0, a)
  Jmp          a -> encode (20, Register 0, Register 0, a)
  Cjmp         a -> encode (21, Register 0, Register 0, a)
  Cnjmp        a -> encode (22, Register 0, Register 0, a)
  Storeb a ri    -> encode (26, ri, Register 0, a)
  Loadb ri a     -> encode (27, ri, Register 0, a)
  Storew a ri    -> encode (28, ri, Register 0, a)
  Loadw ri a     -> encode (29, ri, Register 0, a)
  Read ri a      -> encode (30, ri, Register 0, a)
  Answer a       -> encode (31, Register 0, Register 0, a)

  where
    encode = encodeInstruction' w k


encodeInstruction' :: WordSize -> RegisterCount -> (Int, Register, Register, ImmediateOrRegister) -> (Word, Word)
encodeInstruction' w k (opcode, ri, rj, a) =
  (
    Word . fromIntegral $
    (opcode `shift` (fromIntegral w - 5))
    .|.
    (isImmediate a `shift` (fromIntegral w - 6))
    .|.
    ((ri ^.  #unRegister) `shift` (fromIntegral w - (6 + bitsPerRegister k)))
    .|.
    ((rj ^.  #unRegister) `shift` (fromIntegral w - (6 + 2 * bitsPerRegister k)))
  ,
    Word $ aVal a
  )
  where
    isImmediate x =
      case x of
        IsImmediate _ -> 1
        IsRegister _  -> 0
    aVal :: ImmediateOrRegister -> Integer
    aVal x =
      case x of
        IsImmediate wo          -> fromIntegral wo
        IsRegister (Register r) -> fromIntegral r
