{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.Register (Register (..)) where

import TinyRAM.Prelude

-- A register, represented by its zero-based index.
newtype Register = Register {unRegister :: Int}
  deriving stock (Eq, Ord, Read, Show, Generic)
