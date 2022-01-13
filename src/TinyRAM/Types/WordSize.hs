{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.WordSize ( WordSize (..) ) where


import TinyRAM.Prelude


-- The word size, a positive integer.
newtype WordSize = WordSize { unWordSize :: Int }
  deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral)
