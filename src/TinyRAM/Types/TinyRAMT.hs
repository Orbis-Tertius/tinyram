{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.TinyRAMT (TinyRAMT (..)) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.State (get)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State (StateT, gets, modify)
import qualified Data.Map as Map
import TinyRAM.Prelude
import TinyRAM.Types.Address
import TinyRAM.Types.HasMachineState
  ( Error (..),
    HasMachineState (..),
  )
import TinyRAM.Types.HasParams (HasParams (getParams))
import TinyRAM.Types.Instruction
import TinyRAM.Types.MachineState (MachineState)
import TinyRAM.Types.MemoryValues (MemoryValues (..))
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.RegisterValues (RegisterValues (..))

newtype TinyRAMT m a = TinyRAMT {unTinyRAMT :: StateT (Params, MachineState) (ExceptT Error m) a}
  deriving stock (Generic)

instance MonadTrans TinyRAMT where
  lift = TinyRAMT . lift . lift

instance Functor m => Functor (TinyRAMT m) where
  fmap f = TinyRAMT . fmap f . unTinyRAMT

instance Monad m => Applicative (TinyRAMT m) where
  pure = TinyRAMT . pure
  (TinyRAMT f) <*> (TinyRAMT a) = TinyRAMT $ f <*> a

instance Monad m => Monad (TinyRAMT m) where
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
    maybeWord <- gets (Map.lookup r . (^. _2 . #registerValues . #unRegisterValues))
    case maybeWord of
      Just word -> pure word
      _ -> throwError InvalidRegisterError
  setRegisterValue r w =
    TinyRAMT $
      modify
        ( \s ->
            _2 . #registerValues
              .~ RegisterValues
                ( Map.insert
                    r
                    w
                    (s ^. _2 . #registerValues . #unRegisterValues)
                )
              $ s
        )
  getConditionFlag = TinyRAMT $ gets (^. _2 . #conditionFlag)
  setConditionFlag flag = TinyRAMT $ modify (_2 . #conditionFlag .~ flag)
  fetchInstruction addr =
    TinyRAMT $ do
      s <- get
      let m :: Map Address Instruction
          m = s ^. _2 . #programMemoryValues . #unProgramMemoryValues
       in case Map.lookup addr m of
            Just instruction -> pure instruction
            Nothing -> lift $ throwError InstructionFetchError
  getWord addr =
    TinyRAMT $
      gets
        (Map.findWithDefault 0 addr . (^. _2 . #memoryValues . #unMemoryValues))
  setWord addr w =
    TinyRAMT $
      modify
        ( \s ->
            _2 . #memoryValues
              .~ MemoryValues (Map.insert addr w (s ^. _2 . #memoryValues . #unMemoryValues))
              $ s
        )
  consoleOut c =
    TinyRAMT $
      modify
        ( \s ->
            _2 . #stdout
              .~ c :
            (s ^. _2 . #stdout)
              $ s
        )
