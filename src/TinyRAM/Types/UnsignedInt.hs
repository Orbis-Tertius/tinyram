{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.UnsignedInt ( UnsignedInt (..) ) where


import TinyRAM.Prelude
import TinyRAM.Types.Word (Word)


newtype UnsignedInt = UnsignedInt { unUnsignedInt :: Word }
  deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral)
