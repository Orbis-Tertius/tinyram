{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module TinyRAM.Types.Register ( Register (..) ) where

import           ConCat.Circuit
import           ConCat.Rep

import           TinyRAM.Prelude


-- A register, represented by its zero-based index.
newtype Register = Register { unRegister :: Int }
  deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral)

instance HasRep Register where
  type Rep Register = Int
  repr (Register a) = a
  abst a =  Register a

instance GenBuses Register where
  genBuses' = genBusesRep'
  ty = tyRep @Register
  unflattenB' = genUnflattenB'
