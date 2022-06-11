{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}


module TinyRAM.Types.MachineState ( MachineState (MachineState) ) where


import           TinyRAM.Prelude
import           TinyRAM.Types.Flag                (Flag)
import           TinyRAM.Types.InputTape           (Auxiliary, InputTape, Primary)
import           TinyRAM.Types.MemoryValues        (MemoryValues)
import           TinyRAM.Types.ProgramMemoryValues (ProgramMemoryValues)
import           TinyRAM.Types.ProgramCounter      (ProgramCounter)
import           TinyRAM.Types.RegisterValues      (RegisterValues)


data MachineState =
  MachineState
  { programCounter      :: ProgramCounter
  , registerValues      :: RegisterValues
  , conditionFlag       :: Flag
  -- memory in this VM covers the whole address space
  -- based on the word size, with all memory
  -- implicitly initialized to zero on program start.
  , memoryValues        :: MemoryValues
  , programMemoryValues :: ProgramMemoryValues
  , primaryInput        :: InputTape Primary
  , auxiliaryInput      :: InputTape Auxiliary
  }
  deriving (Eq, Ord, Read, Show, Generic)
