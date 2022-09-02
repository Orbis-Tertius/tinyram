{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.RegisterCount (RegisterCount (..)) where

import TinyRAM.Prelude

-- The total number of registers, a positive integer.
newtype RegisterCount = RegisterCount {unRegisterCount :: Int}
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving newtype (Enum, Num, Real, Integral)
