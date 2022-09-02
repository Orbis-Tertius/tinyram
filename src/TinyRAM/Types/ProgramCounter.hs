{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.ProgramCounter (ProgramCounter (..)) where

import TinyRAM.Prelude
import TinyRAM.Types.Address (Address)

-- An address representing a value of the program counter.
newtype ProgramCounter = ProgramCounter {unProgramCounter :: Address}
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving newtype (Enum, Num, Real, Integral, Bits)
