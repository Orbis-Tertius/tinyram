{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Prelude
  ( module Prelude
  , module Control.Monad.Trans.Class
  , module Data.Map
  , module GHC.Generics
  ) where


import Prelude hiding (Word)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Map (Map)
import GHC.Generics (Generic)
