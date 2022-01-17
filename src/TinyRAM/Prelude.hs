{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Prelude
  ( module Prelude
  , module Control.Lens
  , module Control.Monad.Trans.Class
  , module Data.Bits
  , module Data.ByteString
  , module Data.Map
  , module Data.Maybe
  , module GHC.Generics
  , (<$$>)
  ) where


import Prelude hiding (Word)
import Control.Lens ((^.), (.~), _1, _2)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bits (Bits ((.&.), (.|.), xor, complement, shift))
import Data.ByteString (ByteString)
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)


(<$$>) :: ( Functor f, Functor g ) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap f <$> x
