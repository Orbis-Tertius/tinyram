{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (IsImmediate, IsRegister)) where

import TinyRAM.Prelude
import TinyRAM.Types.Register (Register)
import TinyRAM.Types.Word (Word)

data ImmediateOrRegister
  = IsImmediate Word
  | IsRegister Register
  deriving stock (Eq, Ord, Read, Show, Generic)
