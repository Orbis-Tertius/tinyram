{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Run (run) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import TinyRAM.Bytes (bytesPerWord)
import TinyRAM.Cast (intToAddress)
import TinyRAM.ExecuteInstruction (executeInstruction)
import TinyRAM.MachineState (getImmediateOrRegister)
import TinyRAM.Params (getWordSize)
import TinyRAM.Prelude
import TinyRAM.Types.Address
import TinyRAM.Types.HasMachineState
  ( Error (InfiniteLoopError, InvalidPCAlignment),
    HasMachineState (..),
  )
import TinyRAM.Types.HasParams (HasParams)
import TinyRAM.Types.ImmediateOrRegister
import TinyRAM.Types.Instruction (Instruction (Answer, Jmp))
import TinyRAM.Types.MaxSteps (MaxSteps (..))
import TinyRAM.Types.ProgramCounter (ProgramCounter (..))
import TinyRAM.Types.Word (Word)

run :: (HasMachineState m, HasParams m) => Maybe MaxSteps -> m (Maybe Word)
run (Just 0) = pure Nothing
run n = do
  ws <- getWordSize
  let bytesPerWord' = bytesPerWord ws
  pc <- unProgramCounter <$> getProgramCounter
  unless (pc `mod` intToAddress bytesPerWord' == 0) (throwError InvalidPCAlignment)
  instruction <- fetchInstruction pc
  case instruction of
    Jmp (IsImmediate w) | Address w == pc -> throwError (InfiniteLoopError pc)
    Answer a -> do
      code <- getImmediateOrRegister a
      pure (Just code)
    otherInstruction -> do
      executeInstruction otherInstruction
      run (subtract 1 <$> n)
