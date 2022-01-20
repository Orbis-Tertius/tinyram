{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}


module TinyRAM.MachineState
  ( getImmediateOrRegister
  , conditionToFlag
  , incrementProgramCounter
  , validateProgramCounter
  , validateRegisterKeys
  , validateRegister
  , validateRegisterValues
  , validateMemoryKeys
  , validateMemoryValues
  , validateInputTape
  , validateMachineState
  , validateWord
  ) where


import qualified Data.Map as Map

import TinyRAM.Prelude
import TinyRAM.Types.Flag (Flag)
import TinyRAM.Types.HasMachineState (HasMachineState (..))
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (IsImmediate, IsRegister))
import TinyRAM.Types.InputTape (InputTape (..))
import TinyRAM.Types.MachineState (MachineState)
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.Register (Register (..))
import TinyRAM.Types.Word (Word)


getImmediateOrRegister :: ( Monad m, HasMachineState m )
  => ImmediateOrRegister -> m (Maybe Word)
getImmediateOrRegister (IsImmediate w) = return (Just w)
getImmediateOrRegister (IsRegister r) = getRegisterValue r


conditionToFlag :: Bool -> Flag
conditionToFlag True = 1
conditionToFlag False = 0


incrementProgramCounter :: ( Monad m, HasMachineState m )
  => m ()
incrementProgramCounter = setProgramCounter . (+1) =<< getProgramCounter


validateMachineState :: Params -> MachineState -> Validation
validateMachineState ps s =
  validateProgramCounter ps s
  <> validateRegisterKeys ps s
  <> validateRegisterValues ps s
  <> validate (s ^. #conditionFlag)
  <> validateMemoryKeys ps s
  <> validateMemoryValues ps s
  <> validateInputTape ps (s ^. #primaryInput)
  <> validateInputTape ps (s ^. #auxiliaryInput)


validateProgramCounter :: Params -> MachineState -> Validation
validateProgramCounter ps s =
  validateWord "Program Counter" ps (s ^. #programCounter)


validateRegisterKeys :: Params -> MachineState -> Validation
validateRegisterKeys ps s =
  mconcat $ validateRegister ps <$> Map.keys (s ^. #registerValues . #unRegisterValues)


validateRegister :: Params -> Register -> Validation
validateRegister ps (Register r) =
  if 0 <= r && r < ps ^. #registerCount . #unRegisterCount
  then mempty
  else Validation [Violated "0 <= Register < Register Count"]


validateRegisterValues :: Params -> MachineState -> Validation
validateRegisterValues ps s =
  mconcat $ validateWord "Register Value" ps <$> Map.elems (s ^. #registerValues . #unRegisterValues)


validateMemoryKeys :: Params -> MachineState -> Validation
validateMemoryKeys ps s =
  mconcat $ validateWord "Memory Address" ps <$> Map.keys (s ^. #memoryValues . #unMemoryValues)


validateMemoryValues :: Params -> MachineState -> Validation
validateMemoryValues ps s =
  mconcat $ validateWord "Memory Value" ps <$> Map.elems (s ^. #memoryValues . #unMemoryValues)


validateInputTape :: Params -> InputTape a -> Validation
validateInputTape ps (InputTape t) =
  mconcat $ validateWord "Input Tape" ps <$> t


validateWord :: Integral a => String -> Params -> a -> Validation
validateWord name ps w =
  let ws = ps ^. #wordSize . #unWordSize
  in if 0 <= w && w < 2 ^ ws
     then mempty
     else Validation [Violated $ name <> " must be in the interval [0, 2^" <> show ws <> ")"]
