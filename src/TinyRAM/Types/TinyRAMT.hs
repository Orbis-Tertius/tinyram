{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.TinyRAMT ( TinyRAMT (..) ) where


import Control.Monad.Trans.State (StateT)

import TinyRAM.Prelude
import TinyRAM.Types.MachineState (MachineState)


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
