{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.Flag (Flag (..)) where

import TinyRAM.Prelude

-- A flag value (either 1 or 0).
newtype Flag = Flag {unFlag :: Int}
  deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral)

instance Bounded Flag where
  minBound = 0
  maxBound = 1

instance Validity Flag where
  validate (Flag 0) = mempty
  validate (Flag 1) = mempty
  validate (Flag _) = Validation [Violated "Flag must be in the range [0,1]"]
