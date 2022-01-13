{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}


module TinyRAM.Types.TinyRAMT ( TinyRAMT (..) ) where


import Control.Monad.Trans.State (StateT, gets, modify)
import qualified Data.Map as Map

import TinyRAM.Prelude
import TinyRAM.Types.HasMachineState (HasMachineState (..))
import TinyRAM.Types.InputTape (InputTape (InputTape), Primary, Auxiliary)
import TinyRAM.Types.MachineState (MachineState)
import TinyRAM.Types.MemoryValues (MemoryValues (..))
import TinyRAM.Types.RegisterValues (RegisterValues (..))


newtype TinyRAMT m a = TinyRAMT { unTinyRAMT :: StateT MachineState m a }
  deriving Generic

instance MonadTrans TinyRAMT where
  lift = TinyRAMT . lift

instance Functor m => Functor (TinyRAMT m) where
  fmap f = TinyRAMT . fmap f . unTinyRAMT

instance Monad m => Applicative (TinyRAMT m) where
  pure = TinyRAMT . pure
  (TinyRAMT f) <*> (TinyRAMT a) = TinyRAMT $ f <*> a

instance Monad m => Monad (TinyRAMT m) where
  return = TinyRAMT . return
  (TinyRAMT x) >>= f = TinyRAMT $ x >>= (unTinyRAMT . f)

instance Monad m => HasMachineState (TinyRAMT m) where
  getProgramCounter = TinyRAMT $ gets (^. #programCounter)
  setProgramCounter pc = TinyRAMT $ modify (#programCounter .~ pc)
  getRegisterValue r = TinyRAMT $ gets (Map.lookup r . (^. #registerValues . #unRegisterValues))
  setRegisterValue r w =
    TinyRAMT $ modify
      (\s -> #registerValues
        .~ RegisterValues (Map.insert r w (s ^. #registerValues . #unRegisterValues))
        $ s
      )
  getConditionFlag = TinyRAMT $ gets (^. #conditionFlag)
  setConditionFlag flag = TinyRAMT $ modify (#conditionFlag .~ flag)
  getMemoryValue addr = TinyRAMT $ gets (Map.lookup addr . (^. #memoryValues . #unMemoryValues))
  setMemoryValue addr w =
    TinyRAMT $  modify
      (\s -> #memoryValues
         .~ MemoryValues (Map.insert addr w (s ^. #memoryValues . #unMemoryValues))
         $ s
       )
  readPrimaryInput = TinyRAMT $ do
    input <- gets (^. #primaryInput . #unInputTape)
    case input of
      [] -> return Nothing
      (w:input') -> do
        modify (#primaryInput .~ InputTape @Primary input')
        return (Just w)
  readAuxiliaryInput = TinyRAMT $ do
    input <- gets (^. #auxiliaryInput . #unInputTape)
    case input of
      [] -> return Nothing
      (w:input') -> do
        modify (#auxiliaryInput .~ InputTape @Auxiliary input')
        return (Just w)
