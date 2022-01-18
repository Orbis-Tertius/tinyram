{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.WordSize ( WordSize (..) ) where


import TinyRAM.Prelude


-- The word size, a positive integer.
newtype WordSize = WordSize { unWordSize :: Int }
  deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral)

instance Validity WordSize where
  validate (WordSize ws) =
    if ws <= 0
    then Validation [Violated "Word size must be positive"]
    else if ws `mod` 8 == 0
         then mempty
         else Validation [Violated "Word size must be a multiple of 8"]
