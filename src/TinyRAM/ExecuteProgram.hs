{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.ExecuteProgram ( executeProgram ) where


import Control.Monad.Trans.State (StateT (runStateT))
import Data.Functor.Identity (Identity (runIdentity))

import TinyRAM.Run (run)
import TinyRAM.Prelude
import TinyRAM.Types.InputTape (InputTape, Primary, Auxiliary)
import TinyRAM.Types.MachineState (MachineState)
import TinyRAM.Types.MaxSteps (MaxSteps)
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.Program (Program)
import TinyRAM.Types.TinyRAMT (TinyRAMT (..))
import TinyRAM.Types.Word (Word)


executeProgram
  :: Params
  -> Maybe MaxSteps
  -> Program
  -> InputTape Primary
  -> InputTape Auxiliary
  -> Maybe Word
executeProgram params maxSteps program primaryInput auxInput =
  fst . runIdentity
  $
  runStateT
  (unTinyRAMT (run maxSteps))
  (params, initialMachineState params program primaryInput auxInput)


initialMachineState 
  :: Params
  -> Program
  -> InputTape Primary
  -> InputTape Auxiliary
  -> MachineState
initialMachineState = todo


todo :: a
todo = todo
