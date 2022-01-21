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


import           Control.Lens              (_1, _2, at, (.~), (^.))
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Bits                 (Bits (complement, shift, xor, (.&.), (.|.)))
import           Data.ByteString           (ByteString)
import           Data.Generics.Labels      ()
import           Data.Map                  (Map)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import           Data.Validity             (Validation (Validation, unValidation),
                                            ValidationChain (Location, Violated),
                                            Validity (validate))
import           Data.Validity.ByteString  ()
import           GHC.Generics              (Generic)
import           Prelude                   hiding (Word)
import           System.IO                 (FilePath)


(<$$>) :: ( Functor f, Functor g ) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap f <$> x
