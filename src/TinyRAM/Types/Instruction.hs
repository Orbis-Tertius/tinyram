{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module TinyRAM.Types.Instruction ( Instruction (Instruction) ) where


import           ConCat.Circuit
import           ConCat.Rep
import           TinyRAM.Prelude
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister)
import           TinyRAM.Types.Opcode              (Opcode)
import           TinyRAM.Types.Register            (Register)


data Instruction =
  Instruction
  { opcode :: Opcode
  , a      :: ImmediateOrRegister
  , ri     :: Register
  , rj     :: Register
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance HasRep Instruction where
  type Rep Instruction = (Opcode, ImmediateOrRegister, Register, Register)
  repr (Instruction a' b c d) = (a', b, c, d)
  abst (a', b, c, d)= (Instruction a' b c d)

instance GenBuses Instruction where
  genBuses' = genBusesRep'
  ty = tyRep @Instruction
  unflattenB' = genUnflattenB'
