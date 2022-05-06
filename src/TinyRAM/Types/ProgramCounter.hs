{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}


module TinyRAM.Types.ProgramCounter ( ProgramCounter (..) ) where


import           TinyRAM.Prelude
import           TinyRAM.Types.Address (Address)


-- An address representing a value of the program counter.
newtype ProgramCounter = ProgramCounter { unProgramCounter :: Address }
  deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral, Bits)
