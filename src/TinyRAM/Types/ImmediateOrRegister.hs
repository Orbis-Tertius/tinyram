{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (IsImmediate, IsRegister)) where


import           ConCat.Circuit
import           ConCat.Misc
import           ConCat.Rep
import           TinyRAM.Prelude
import           TinyRAM.Types.Register (Register)
import           TinyRAM.Types.Word     (Word)


data ImmediateOrRegister =
    IsImmediate Word
  | IsRegister Register
  deriving (Eq, Ord, Read, Show, Generic)

instance HasRep ImmediateOrRegister where
  type Rep ImmediateOrRegister = Word :+ Register
  repr (IsImmediate a) = Left a
  repr (IsRegister b)  = Right b
  abst (Left a)  = IsImmediate a
  abst (Right b) = IsRegister b

instance GenBuses ImmediateOrRegister where
  genBuses' = genBusesRep'
  ty = tyRep @ImmediateOrRegister
  unflattenB' = genUnflattenB'
