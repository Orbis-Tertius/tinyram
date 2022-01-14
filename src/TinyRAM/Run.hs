{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}


module TinyRAM.Run ( run ) where


import TinyRAM.DecodeInstruction (decodeInstruction)
import TinyRAM.ExecuteInstruction (executeInstruction)
import TinyRAM.MachineState (getImmediateOrRegister)
import TinyRAM.Prelude
import TinyRAM.Types.HasMachineState (HasMachineState (..))
import TinyRAM.Types.HasParams (HasParams)
import TinyRAM.Types.ProgramCounter (ProgramCounter (..))
import TinyRAM.Types.Word (Word)


run :: ( Monad m, HasMachineState m, HasParams m ) => m Word
run = do
  pc <- unProgramCounter <$> getProgramCounter
  i0 <- getMemoryValue pc
  i1 <- getMemoryValue (pc+1)
  case (i0, i1) of
    (Just i0', Just i1') ->
      let i = decodeInstruction (i0', i1') in
        if i ^. #opcode == 31
        then do
          a <- getImmediateOrRegister (i ^. #a)
          case a of
            Just a' -> return a'
            _ -> run
        else do
          executeInstruction i
          run
    _ -> run
