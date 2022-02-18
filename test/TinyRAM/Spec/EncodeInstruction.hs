{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}


module TinyRAM.Spec.EncodeInstruction ( encodeInstruction ) where


import           TinyRAM.DecodeInstruction         (bitsPerRegister)
import           TinyRAM.Prelude
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (IsImmediate, IsRegister))
import           TinyRAM.Types.Instruction         (Instruction)
import           TinyRAM.Types.RegisterCount       (RegisterCount)
import           TinyRAM.Types.Word                (Word (..))


encodeInstruction :: RegisterCount -> Instruction -> (Word, Word)
encodeInstruction rc i =
  ( Word $ fromIntegral (i ^. #opcode . #unOpcode)
       .|. aBit `shift` 5
       .|. fromIntegral (i ^. #ri . #unRegister) `shift` 6
       .|. fromIntegral (i ^. #rj . #unRegister) `shift` (6 + bitsPerRegister rc)
  , aVal
  )
  where
    aBit =
      case i ^. #a of
        IsImmediate _ -> 2 ^ (5 :: Integer)
        IsRegister  _ -> 0
    aVal =
      case i ^. #a of
        IsImmediate w -> w
        IsRegister r  -> fromIntegral r
