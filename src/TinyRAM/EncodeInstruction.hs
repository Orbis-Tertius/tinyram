{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
module TinyRAM.EncodeInstruction (encodeInstruction, instructionToDword) where

import           TinyRAM.Prelude
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (..))
import           TinyRAM.Types.Instruction         (Instruction)
import           TinyRAM.Types.RegisterCount       (RegisterCount (..))
import           TinyRAM.Types.Word                (Word (..))
import           TinyRAM.Types.WordSize            (WordSize (..))

import           TinyRAM.DecodeInstruction         (bitsPerRegister)

encodeInstruction :: Instruction -> WordSize -> RegisterCount -> Word
encodeInstruction instr ws rc =
  Word $
  aVal (instr ^. #a)
  .|.
  (fromIntegral (instr ^. #rj . #unRegister) `shift` (2 * fromIntegral ws - 2 * bitsPerRegister rc - bitsPerOpcode - 1))
  .|.
  (fromIntegral (instr ^. #ri . #unRegister) `shift` (2 * fromIntegral ws - 1 * bitsPerRegister rc - bitsPerOpcode - 1))
  .|.
  (isImmediate (instr ^. #a) `shift` (2 * fromIntegral ws - bitsPerOpcode - 1))
  .|.
  (fromIntegral (instr ^. #opcode . #unOpcode) `shift` (2 * fromIntegral ws - bitsPerOpcode))
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

    bitsPerOpcode :: Int
    bitsPerOpcode = 5

instructionToDword :: WordSize -> RegisterCount -> Instruction -> (Word, Word)
instructionToDword ws rc instruction =
  (dword .&. (2 ^ ws - 1), (dword `shift` (- fromIntegral ws)) .&. (2 ^ ws - 1))
  where
    dword = encodeInstruction instruction ws rc
