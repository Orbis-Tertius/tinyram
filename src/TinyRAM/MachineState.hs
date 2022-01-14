{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.MachineState
  ( getImmediateOrRegister
  , conditionToFlag
  , incrementProgramCounter
  ) where


import TinyRAM.Prelude
import TinyRAM.Types.Flag (Flag)
import TinyRAM.Types.HasMachineState (HasMachineState (..))
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (IsImmediate, IsRegister))
import TinyRAM.Types.Word (Word)


getImmediateOrRegister :: ( Monad m, HasMachineState m )
  => ImmediateOrRegister -> m (Maybe Word)
getImmediateOrRegister (IsImmediate w) = return (Just w)
getImmediateOrRegister (IsRegister r) = getRegisterValue r


conditionToFlag :: Bool -> Flag
conditionToFlag True = 1
conditionToFlag False = 0


incrementProgramCounter :: ( Monad m, HasMachineState m )
  => m ()
incrementProgramCounter = setProgramCounter . (+1) =<< getProgramCounter
