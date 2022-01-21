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
import TinyRAM.Types.HasParams (HasParams (getParams))
import TinyRAM.Types.InputTape (InputTape (InputTape), Primary, Auxiliary)
import TinyRAM.Types.MachineState (MachineState)
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.MemoryValues (MemoryValues (..))
import TinyRAM.Types.RegisterValues (RegisterValues (..))


newtype TinyRAMT m a = TinyRAMT { unTinyRAMT :: StateT (Params, MachineState) m a }
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

instance Monad m => HasParams (TinyRAMT m) where
  getParams = TinyRAMT $ gets (^. _1)

instance Monad m => HasMachineState (TinyRAMT m) where
  getProgramCounter = TinyRAMT $ gets (^. _2 . #programCounter)
  setProgramCounter pc = TinyRAMT $ modify (_2 . #programCounter .~ pc)
  getRegisterValue r = TinyRAMT $ gets (Map.lookup r . (^. _2 . #registerValues . #unRegisterValues))
  setRegisterValue r w =
    TinyRAMT $ modify
      (\s -> _2 . #registerValues
        .~ RegisterValues (Map.insert r w
             (s ^. _2 . #registerValues . #unRegisterValues))
        $ s
      )
  getConditionFlag = TinyRAMT $ gets (^. _2 . #conditionFlag)
  setConditionFlag flag = TinyRAMT $ modify (_2 . #conditionFlag .~ flag)
  getMemoryValue addr =
    TinyRAMT $ gets
      (Map.findWithDefault 0 addr . (^. _2 . #memoryValues . #unMemoryValues))
  setMemoryValue addr w =
    TinyRAMT $  modify
      (\s -> _2 . #memoryValues
         .~ MemoryValues (Map.insert addr w (s ^. _2 . #memoryValues . #unMemoryValues))
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
