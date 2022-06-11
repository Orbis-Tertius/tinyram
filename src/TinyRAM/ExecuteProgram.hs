{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module TinyRAM.ExecuteProgram ( executeProgram ) where


import           Control.Monad.Trans.Except   (runExceptT)
import           Control.Monad.Trans.State    (StateT (runStateT))
import qualified Data.Bifunctor               as Bi
import           Data.Functor.Identity        (Identity (runIdentity))
import qualified Data.Map                     as Map
import           Data.Text                    (pack)

import           TinyRAM.Bytes                (bytesPerWord, bytesToWords)
import           TinyRAM.Prelude
import           TinyRAM.Run                  (run)
import           TinyRAM.Types.Flag           (Flag)
import           TinyRAM.Types.InputTape      (Auxiliary, InputTape, Primary)
import           TinyRAM.Types.MachineState   (MachineState (..))
import           TinyRAM.Types.MaxSteps       (MaxSteps)
import           TinyRAM.Types.MemoryValues   (MemoryValues (..))
import           TinyRAM.Types.Params         (Params)
import           TinyRAM.Types.Program        (Program (..))
import TinyRAM.Types.ProgramMemoryValues (ProgramMemoryValues (..))
import           TinyRAM.Types.ProgramCounter (ProgramCounter)
import           TinyRAM.Types.Register       (Register (..))
import           TinyRAM.Types.RegisterCount  (RegisterCount (..))
import           TinyRAM.Types.RegisterValues (RegisterValues (..))
import           TinyRAM.Types.TinyRAMT       (TinyRAMT (..))
import           TinyRAM.Types.Word           (Word)

executeProgram
  :: Params
  -> Maybe MaxSteps
  -> Program
  -> InputTape Primary
  -> InputTape Auxiliary
  -> Either Text Word
executeProgram params maxSteps program primaryInput auxInput = do
  programMemoryValues <- programToMemoryValues params program
  let result =
        runIdentity $
        runExceptT $
        runStateT
        (unTinyRAMT (run maxSteps))
        (params, initialMachineState params (MemoryValues mempty) programMemoryValues primaryInput auxInput)
  (maybeWord, _ ) <- Bi.first (pack . show) result
  maybe (Left $ "program did not terminate in " <> pack (show maxSteps)) Right maybeWord


initialMachineState
  :: Params
  -> MemoryValues
  -> ProgramMemoryValues
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
  -> Either Text ProgramMemoryValues
programToMemoryValues params (Program p) =
  if params ^. #wordSize == 0
  then Left "word size must be nonzero but it is zero"
  else
    case (params ^. #wordSize . #unWordSize) `rem` 8 of
      0 ->
        Right . ProgramMemoryValues . Map.fromList $ zip addresses (bytesToWords (params ^. #wordSize) p)
      _ -> Left $ "word size must be a multiple of 8 but it is " <> pack (show (params ^. #wordSize . #unWordSize))
  where
    bytesPerWord' = bytesPerWord (params ^. #wordSize)

    addresses = (* fromIntegral bytesPerWord') <$> [0..]
