{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
module TinyRAM.EncodeInstruction (encodeInstruction) where

import           TinyRAM.Prelude
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (..))
import           TinyRAM.Types.Instruction         (Instruction)
import           TinyRAM.Types.RegisterCount       (RegisterCount (..))
import           TinyRAM.Types.Word                (Word (..))
import           TinyRAM.Types.WordSize            (WordSize (..))

import           TinyRAM.DecodeInstruction         (bitsPerRegister)

encodeInstruction :: WordSize -> RegisterCount -> Instruction -> (Word, Word)
encodeInstruction w k instr =
  (
    Word . fromIntegral $
    ((instr ^. #opcode . #unOpcode) `shift` (fromIntegral w - 5))
    .|.
    (isImmediate (instr ^. #a) `shift` (fromIntegral w - 6))
    .|.
    (((instr ^. #ri . #unRegister)) `shift` (fromIntegral w - (6 + bitsPerRegister k)))
    .|.
    (((instr ^. #rj . #unRegister)) `shift` (fromIntegral w - (6 + 2 * bitsPerRegister k)))
  ,
    Word $ aVal (instr ^. #a)
  )
  where
    isImmediate x =
      case x of
        IsImmediate _ -> 1
        IsRegister _  -> 0
    aVal :: ImmediateOrRegister -> Integer
    aVal x =
      case x of
        IsImmediate wo -> fromIntegral wo
        IsRegister r   -> fromIntegral r
