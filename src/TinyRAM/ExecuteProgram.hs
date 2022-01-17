{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


module TinyRAM.ExecuteProgram ( executeProgram ) where


import Control.Monad.Trans.State (StateT (runStateT))
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Map as Map
import Data.Text (pack)

import TinyRAM.Run (run)
import TinyRAM.Prelude
import TinyRAM.Types.Flag (Flag)
import TinyRAM.Types.InputTape (InputTape, Primary, Auxiliary)
import TinyRAM.Types.MachineState (MachineState (..))
import TinyRAM.Types.MaxSteps (MaxSteps)
import TinyRAM.Types.MemoryValues (MemoryValues)
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.Program (Program)
import TinyRAM.Types.ProgramCounter (ProgramCounter)
import TinyRAM.Types.Register (Register (..))
import TinyRAM.Types.RegisterCount (RegisterCount (..))
import TinyRAM.Types.RegisterValues (RegisterValues (..))
import TinyRAM.Types.TinyRAMT (TinyRAMT (..))
import TinyRAM.Types.Word (Word)


executeProgram
  :: Params
  -> Maybe MaxSteps
  -> Program
  -> InputTape Primary
  -> InputTape Auxiliary
  -> Either Text Word
executeProgram params maxSteps program primaryInput auxInput = do
  memoryValues <- programToMemoryValues params program
  maybe (Left $ "program did not terminate in " <> pack (show maxSteps)) Right
    . fst . runIdentity
    $
    runStateT
    (unTinyRAMT (run maxSteps))
    (params, initialMachineState params memoryValues primaryInput auxInput)


initialMachineState 
  :: Params
  -> MemoryValues
  -> InputTape Primary
  -> InputTape Auxiliary
  -> MachineState
initialMachineState params =
  MachineState
  (0 :: ProgramCounter)
  (initialRegisterValues (params ^. #registerCount))
  (0 :: Flag)


initialRegisterValues :: RegisterCount -> RegisterValues
initialRegisterValues (RegisterCount n) =
  RegisterValues . Map.fromList
  $ (,0) . Register <$> [0..n-1]


programToMemoryValues
  :: Params
  -> Program
  -> Either Text MemoryValues
programToMemoryValues = todo


todo :: a
todo = todo
