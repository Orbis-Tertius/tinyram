{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.Opcode ( Opcode (..) ) where


import TinyRAM.Prelude


newtype Opcode = Opcode { unOpcode :: Int }
  deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral)

instance Bounded Opcode where
  minBound = 0
  maxBound = 31

instance Validity Opcode where
  validate o =
    if (0 <= o && o <= 22) || (28 <= o && o <= 30)
    then mempty
    else Validation [Violated "Opcode must be valid"]
