{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.ProgramMemoryValues (ProgramMemoryValues (..)) where

import TinyRAM.Prelude
import TinyRAM.Types.Address (Address)
import TinyRAM.Types.Instruction (Instruction)

-- A state of the RAM, giving the value at each address.
newtype ProgramMemoryValues = ProgramMemoryValues
  {unProgramMemoryValues :: Map Address Instruction}
  deriving (Eq, Ord, Read, Generic)

instance Show ProgramMemoryValues where
  show _ = "<ProgramMemoryValues>"
