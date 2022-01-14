{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.Sign ( Sign (..) ) where


import TinyRAM.Prelude


-- An integral sign (either -1 or 1). 1 indicates non-negative; -1 indicates negative.
newtype Sign = Sign { unSign :: Int }
  deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral)

instance Bounded Sign where
  minBound = -1
  maxBound = 1
