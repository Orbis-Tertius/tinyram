{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.MachineState ( MachineState (MachineState) ) where


import TinyRAM.Prelude
import TinyRAM.Types.Flag (Flag)
import TinyRAM.Types.MemoryValues (MemoryValues)
import TinyRAM.Types.InputTape (InputTape, Primary, Auxiliary)
import TinyRAM.Types.ProgramCounter (ProgramCounter)
import TinyRAM.Types.RegisterValues (RegisterValues)


data MachineState =
  MachineState
  { programCounter :: ProgramCounter
  , registerValues :: RegisterValues
  , conditionFlag  :: Flag
  , memoryValues   :: MemoryValues
  , primaryInput   :: InputTape Primary
  , auxiliaryInput :: InputTape Auxiliary
  }
  deriving (Eq, Ord, Read, Show, Generic)
