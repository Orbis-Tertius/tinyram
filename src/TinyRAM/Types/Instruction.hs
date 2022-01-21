{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.Instruction ( Instruction (Instruction) ) where


import           TinyRAM.Prelude
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister)
import           TinyRAM.Types.Opcode              (Opcode)
import           TinyRAM.Types.Register            (Register)


data Instruction =
  Instruction
  { opcode :: Opcode
  , a      :: ImmediateOrRegister
  , ri     :: Register
  , rj     :: Register
  }
  deriving (Eq, Ord, Read, Show, Generic)
