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

encodeInstruction :: Instruction -> WordSize -> RegisterCount -> Word
encodeInstruction instr w k =
  Word $
  (fromIntegral (instr ^. #opcode . #unOpcode))
   .|.
  (isImmediate (instr ^. #a) `shift` 5)
   .|.
  ((fromIntegral $ (instr ^. #ri . #unRegister)) `shift` 6)
   .|.
  ((fromIntegral $ (instr ^. #rj . #unRegister)) `shift` (6 + bitsPerRegister k))
   .|.
   (aVal (instr ^. #a) `shift` (fromIntegral w))

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
