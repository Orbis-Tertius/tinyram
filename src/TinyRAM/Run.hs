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
import           TinyRAM.Types.HasMachineState (Error (InvalidPCAlignment),
                                                HasMachineState (..))
import           TinyRAM.Types.HasParams       (HasParams)
import           TinyRAM.Types.MaxSteps        (MaxSteps (..))
import           TinyRAM.Types.ProgramCounter  (ProgramCounter (..))
import           TinyRAM.Types.Word            (Word)
import           TinyRAM.Types.WordSize        (WordSize (..))

run :: ( HasMachineState m, HasParams m ) => Maybe MaxSteps -> m (Maybe Word)
run (Just 0) = return Nothing
run n = do
  rc <- getRegisterCount
  ws <- getWordSize
  let  bytesPerWord' = bytesPerWord ws
  pc <- unProgramCounter <$> getProgramCounter
  unless (pc `mod` fromIntegral bytesPerWord' == 0) (throwError InvalidPCAlignment )
  i0 <- getProgramWord pc
  i1 <- getProgramWord ((pc + fromIntegral bytesPerWord') `mod` (2 ^ unWordSize ws))
  let i = decodeInstruction ws rc (i0, i1) in
    if i ^. #opcode == 31
    then do
      a <- getImmediateOrRegister (i ^. #a)
      return (Just a)
    else
      do
      executeInstruction i
      run (subtract 1 <$> n)
