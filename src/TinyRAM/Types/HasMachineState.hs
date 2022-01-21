{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.HasMachineState ( HasMachineState (..) ) where


import TinyRAM.Prelude
import TinyRAM.Types.Address (Address)
import TinyRAM.Types.Flag (Flag)
import TinyRAM.Types.ProgramCounter (ProgramCounter)
import TinyRAM.Types.Register (Register)
import TinyRAM.Types.Word (Word)


class HasMachineState m where
  getProgramCounter :: m ProgramCounter
  setProgramCounter :: ProgramCounter -> m ()
  getRegisterValue :: Register -> m (Maybe Word)
  setRegisterValue :: Register -> Word -> m ()
  getConditionFlag :: m Flag
  setConditionFlag :: Flag -> m ()
  getMemoryValue :: Address -> m Word
  setMemoryValue :: Address -> Word -> m ()
  readPrimaryInput :: m (Maybe Word)
  readAuxiliaryInput :: m (Maybe Word)
