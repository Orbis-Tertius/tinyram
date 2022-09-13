{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.WordSize (WordSize (..)) where

import TinyRAM.Prelude

-- The word size, a positive integer.
newtype WordSize = WordSize {unWordSize :: Int}
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving newtype (Enum, Num, Real, Integral)

instance Validity WordSize where
  validate (WordSize ws)
    | ws <= 0 = Validation [Violated "Word size must be positive"]
    | ws `mod` 8 == 0 = mempty
    | otherwise = Validation [Violated "Word size must be a multiple of 8"]
