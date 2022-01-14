{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}


module TinyRAM.ExecuteInstruction ( executeInstruction ) where


import TinyRAM.Instructions (andBits, orBits)
import TinyRAM.Prelude
import TinyRAM.Types.HasMachineState (HasMachineState (..))
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister)
import TinyRAM.Types.Instruction (Instruction)
import TinyRAM.Types.Register (Register)


executeInstruction :: ( Monad m, HasMachineState m )
  => Instruction -> m ()
executeInstruction i =
  case i ^. #opcode of
    0 -> threeArgOpcode andBits i
    1 -> threeArgOpcode orBits i
    _ -> return ()


threeArgOpcode :: (Register -> Register -> ImmediateOrRegister -> a) -> Instruction -> a
threeArgOpcode f i =
  f (i ^. #ri) (i ^. #rj) (i ^. #a)
