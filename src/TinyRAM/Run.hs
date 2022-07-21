{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}


module TinyRAM.Run ( run ) where


import           Control.Monad                 (unless)
import           Control.Monad.Except          (throwError)

import           TinyRAM.Bytes                 (bytesPerWord)
import           TinyRAM.DecodeInstruction     (decodeInstruction)
import           TinyRAM.ExecuteInstruction    (executeInstruction)
import           TinyRAM.MachineState          (getImmediateOrRegister)
import           TinyRAM.Params                (getRegisterCount, getWordSize)
import           TinyRAM.Prelude
import           TinyRAM.Types.HasMachineState (Error (InvalidBinaryEncoding, InvalidPCAlignment),
                                                HasMachineState (..))
import           TinyRAM.Types.HasParams       (HasParams)
import           TinyRAM.Types.Instruction     (Instruction (Answer))
import           TinyRAM.Types.MaxSteps        (MaxSteps (..))
import           TinyRAM.Types.ProgramCounter  (ProgramCounter (..))
import           TinyRAM.Types.Word            (Word)

run :: ( HasMachineState m, HasParams m ) => Maybe MaxSteps -> m (Maybe Word)
run (Just 0) = return Nothing
run n = do
  rc <- getRegisterCount
  ws <- getWordSize
  let  bytesPerWord' = bytesPerWord ws
  pc <- unProgramCounter <$> getProgramCounter
  unless (pc `mod` fromIntegral bytesPerWord' == 0) (throwError InvalidPCAlignment )
  instruction <- fetchInstruction pc
  case instruction of
    Answer a -> do
      code <- getImmediateOrRegister a
      return (Just code)
    instruction -> do
      executeInstruction instruction
      run (subtract 1 <$> n)
