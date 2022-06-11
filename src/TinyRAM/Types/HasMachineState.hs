{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}


module TinyRAM.Types.HasMachineState ( HasMachineState (..), Error (..) ) where


import           Control.Monad.Except         (MonadError)
import           TinyRAM.Prelude
import           TinyRAM.Types.Address        (Address)
import           TinyRAM.Types.Flag           (Flag)
import           TinyRAM.Types.ProgramCounter (ProgramCounter)
import           TinyRAM.Types.Register       (Register)
import           TinyRAM.Types.Word           (Word)


data Error =
    InvalidOpcodeError
  | InvalidRegisterError
  | InvalidPCAlignment
  deriving (Eq, Show)

class (MonadError Error m) => HasMachineState m where
  getProgramCounter :: m ProgramCounter
  setProgramCounter :: ProgramCounter -> m ()
  getRegisterValue :: Register -> m Word
  setRegisterValue :: Register -> Word -> m ()
  getConditionFlag :: m Flag
  setConditionFlag :: Flag -> m ()
  getWord :: Address -> m Word
  getProgramWord :: Address -> m Word
  setWord :: Address -> Word -> m ()
  setProgramWord :: Address -> Word -> m ()
  readPrimaryInput :: m (Maybe Word)
  readAuxiliaryInput :: m (Maybe Word)
