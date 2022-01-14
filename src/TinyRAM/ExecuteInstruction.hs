{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.ExecuteInstruction ( executeInstruction ) where


import TinyRAM.Prelude
import TinyRAM.Types.HasMachineState (HasMachineState (..))
import TinyRAM.Types.HasParams (HasParams (..))
import TinyRAM.Types.Instruction (Instruction)


executeInstruction :: ( Monad m, HasMachineState m, HasParams m )
  => Instruction -> m ()
executeInstruction _ = do
  _ <- getParams
  _ <- getProgramCounter
  todo


todo :: a
todo = todo
