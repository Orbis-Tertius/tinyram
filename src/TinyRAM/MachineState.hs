{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.MachineState
  ( getImmediateOrRegister
  ) where


import TinyRAM.Prelude
import TinyRAM.Types.HasMachineState (HasMachineState (..))
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (IsImmediate, IsRegister))
import TinyRAM.Types.Word (Word)


getImmediateOrRegister :: ( Monad m, HasMachineState m )
  => ImmediateOrRegister -> m (Maybe Word)
getImmediateOrRegister (IsImmediate w) = return (Just w)
getImmediateOrRegister (IsRegister r) = getRegisterValue r
