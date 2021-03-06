{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module TinyRAM.Types.TinyRAMT ( TinyRAMT (..) ) where


import           Control.Monad.Trans.State         (StateT, gets, modify)
import qualified Data.Map                          as Map

import           Control.Monad.Except              (MonadError (..))
import           Control.Monad.Trans.Except        (ExceptT)
import           TinyRAM.Bytes                     (bytesPerWord)
import           TinyRAM.EncodeInstruction         (encodeInstruction)
import           TinyRAM.Prelude
import           TinyRAM.Types.HasMachineState     (Error (..),
                                                    HasMachineState (..))
import           TinyRAM.Types.HasParams           (HasParams (getParams))
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (IsImmediate))
import           TinyRAM.Types.InputTape           (Auxiliary,
                                                    InputTape (InputTape),
                                                    Primary)
import           TinyRAM.Types.Instruction         (Instruction (..))
import           TinyRAM.Types.MachineState        (MachineState)
import           TinyRAM.Types.MemoryValues        (MemoryValues (..))
import           TinyRAM.Types.Params              (Params)
import           TinyRAM.Types.ProgramMemoryValues (ProgramMemoryValues (..))
import           TinyRAM.Types.Register            (Register (..))
import           TinyRAM.Types.RegisterValues      (RegisterValues (..))
import           TinyRAM.Types.Word                (Word (..))


newtype TinyRAMT m a = TinyRAMT { unTinyRAMT :: StateT (Params, MachineState) (ExceptT Error m) a }
  deriving Generic

instance MonadTrans TinyRAMT where
  lift = TinyRAMT . lift . lift

instance Functor m => Functor (TinyRAMT m) where
  fmap f = TinyRAMT . fmap f . unTinyRAMT

instance Monad m => Applicative (TinyRAMT m) where
  pure = TinyRAMT . pure
  (TinyRAMT f) <*> (TinyRAMT a) = TinyRAMT $ f <*> a

instance Monad m => Monad (TinyRAMT m) where
  return = TinyRAMT . return
  (TinyRAMT x) >>= f = TinyRAMT $ x >>= (unTinyRAMT . f)

instance Monad m => HasParams (TinyRAMT m) where
  getParams = TinyRAMT $ gets (^. _1)

instance Monad m => MonadError Error (TinyRAMT m) where
  throwError = TinyRAMT . throwError
  catchError ma handler = TinyRAMT (catchError (unTinyRAMT ma) (unTinyRAMT . handler))


instance (Monad m, MonadError Error (TinyRAMT m)) => HasMachineState (TinyRAMT m) where
  getProgramCounter = TinyRAMT $ gets (^. _2 . #programCounter)
  setProgramCounter pc = TinyRAMT $ modify (_2 . #programCounter .~ pc)
  getRegisterValue r = TinyRAMT $ do
                          maybeWord <- gets ( Map.lookup r . (^. _2 . #registerValues . #unRegisterValues))
                          case maybeWord of
                            Just word -> return word
                            _         -> throwError InvalidRegisterError
  setRegisterValue r w =
    TinyRAMT $ modify
      (\s -> _2 . #registerValues
        .~ RegisterValues (Map.insert r w
             (s ^. _2 . #registerValues . #unRegisterValues))
        $ s
      )
  getConditionFlag = TinyRAMT $ gets (^. _2 . #conditionFlag)
  setConditionFlag flag = TinyRAMT $ modify (_2 . #conditionFlag .~ flag)
  fetchInstruction addr =
    TinyRAMT . gets $ \s ->
      let m = s ^. _2 . #programMemoryValues. #unProgramMemoryValues
          ws = s ^. _1 . #wordSize
          rc = s ^. _1 . #registerCount
          answer1 = encodeInstruction ws rc
            (Instruction 31
              (IsImmediate (Word 1)) (Register 0) (Register 0))
      in fromMaybe answer1 $
           (,) <$> Map.lookup addr m
               <*> Map.lookup (addr + fromIntegral (bytesPerWord ws)) m
  getWord addr =
    TinyRAMT $ gets
      (Map.findWithDefault 0 addr . (^. _2 . #memoryValues . #unMemoryValues))
  setWord addr w =
    TinyRAMT $ modify
      (\s -> _2 . #memoryValues
         .~ MemoryValues (Map.insert addr w (s ^. _2 . #memoryValues . #unMemoryValues))
         $ s
       )
  setProgramWord addr w =
    TinyRAMT $ modify
      (\s -> _2 . #programMemoryValues
         .~ ProgramMemoryValues (Map.insert addr w
            (s ^. _2 . #programMemoryValues . #unProgramMemoryValues))
         $ s
      )
  readPrimaryInput = TinyRAMT $ do
    input <- gets (^. _2 . #primaryInput . #unInputTape)
    case input of
      [] -> return Nothing
      (w:input') -> do
        modify (_2 . #primaryInput .~ InputTape @Primary input')
        return (Just w)
  readAuxiliaryInput = TinyRAMT $ do
    input <- gets (^. _2 . #auxiliaryInput . #unInputTape)
    case input of
      [] -> return Nothing
      (w:input') -> do
        modify (_2 . #auxiliaryInput .~ InputTape @Auxiliary input')
        return (Just w)
