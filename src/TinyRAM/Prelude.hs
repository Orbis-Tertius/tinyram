{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Prelude
  ( module Prelude
  , module Control.Lens
  , module Control.Monad.Trans.Class
  , module Data.Bits
  , module Data.ByteString
  , module Data.Map
  , module Data.Maybe
  , module Data.Text
  , module Data.Validity
  , module GHC.Generics
  , module System.IO
  , (<$$>)
  ) where


import Prelude hiding (Word)
import Control.Lens ((^.), (.~), _1, _2, at)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bits (Bits ((.&.), (.|.), xor, complement, shift))
import Data.ByteString (ByteString)
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Validity (Validity (validate), Validation (Validation, unValidation), ValidationChain (Violated, Location))
import Data.Validity.ByteString ()
import GHC.Generics (Generic)
import System.IO (FilePath)


(<$$>) :: ( Functor f, Functor g ) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap f <$> x
