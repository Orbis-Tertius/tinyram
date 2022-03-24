{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}


module TinyRAM.Run ( run ) where


import           TinyRAM.Bytes                 (bytesPerWord)
import           TinyRAM.DecodeInstruction     (decodeInstruction)
import           TinyRAM.ExecuteInstruction    (executeInstruction)
import           TinyRAM.MachineState          (getImmediateOrRegister)
import           TinyRAM.Params                (getRegisterCount, getWordSize)
import           TinyRAM.Prelude
import           TinyRAM.Types.HasMachineState (HasMachineState (..))
import           TinyRAM.Types.HasParams       (HasParams)
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
  i0 <- getWord pc
  i1 <- getWord (pc + fromIntegral bytesPerWord')
  let i = decodeInstruction ws rc (i0, i1) in
    if i ^. #opcode == 31
    then do
      a <- getImmediateOrRegister (i ^. #a)
      return (Just a)
    else
      do
      executeInstruction i
      run (subtract 1 <$> n)
