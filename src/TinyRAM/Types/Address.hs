{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.Address (Address (..)) where

import TinyRAM.Prelude
import TinyRAM.Types.Word (Word)

-- A word representing an address.
newtype Address = Address {unAddress :: Word}
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving newtype (Enum, Num, Real, Integral, Bits)
