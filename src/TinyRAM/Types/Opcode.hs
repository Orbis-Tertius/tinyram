{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}


module TinyRAM.Types.Opcode ( Opcode (..) ) where


import           ConCat.Circuit
import           ConCat.Rep
import           TinyRAM.Prelude


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

instance HasRep Opcode where
  type Rep Opcode = Int
  repr (Opcode a) = a
  abst a =  Opcode a

instance GenBuses Opcode where
  genBuses' = genBusesRep'
  ty = tyRep @Opcode
  unflattenB' = genUnflattenB'
