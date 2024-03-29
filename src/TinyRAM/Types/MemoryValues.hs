{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.MemoryValues (MemoryValues (..)) where

import TinyRAM.Prelude
import TinyRAM.Types.Address (Address)
import TinyRAM.Types.Word (Word)

-- A state of the RAM, giving the value at each address.
newtype MemoryValues = MemoryValues
  {unMemoryValues :: Map Address Word}
  deriving stock (Eq, Ord, Read, Generic)

instance Show MemoryValues where
  show _ = "<MemoryValues>"
