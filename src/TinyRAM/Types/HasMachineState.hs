{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.HasMachineState (HasMachineState (..), Error (..)) where

import Control.Monad.Except (MonadError)
import TinyRAM.Prelude
import TinyRAM.Types.Address (Address)
import TinyRAM.Types.Flag (Flag)
import TinyRAM.Types.Instruction (Instruction)
import TinyRAM.Types.ProgramCounter (ProgramCounter)
import TinyRAM.Types.Register (Register)
import TinyRAM.Types.Word (Word)

data Error
  = InstructionFetchError
  | InfiniteLoopError Address
  | InvalidBinaryEncoding
  | InvalidOpcodeError
  | InvalidRegisterError
  | InvalidPCAlignment
  deriving stock (Eq, Show)

class (MonadError Error m) => HasMachineState m where
  getProgramCounter :: m ProgramCounter
  setProgramCounter :: ProgramCounter -> m ()
  getRegisterValue :: Register -> m Word
  setRegisterValue :: Register -> Word -> m ()
  getConditionFlag :: m Flag
  setConditionFlag :: Flag -> m ()
  getWord :: Address -> m Word
  setWord :: Address -> Word -> m ()
  fetchInstruction :: Address -> m Instruction
  readPrimaryInput :: m (Maybe Word)
  readAuxiliaryInput :: m (Maybe Word)
