{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}


module TinyRAM.Types.ProgramMemoryValues ( ProgramMemoryValues (..) ) where


import           TinyRAM.Prelude
import           TinyRAM.Types.Address (Address)
import           TinyRAM.Types.Word    (Word)


-- A state of the RAM, giving the value at each address.
newtype ProgramMemoryValues = ProgramMemoryValues
  { unProgramMemoryValues :: Map Address Word }
  deriving (Eq, Ord, Read, Show, Generic)
