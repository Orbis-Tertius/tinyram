{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Prelude
  ( module Prelude,
    module Control.Lens,
    module Control.Monad.Trans.Class,
    module Data.Bits,
    module Data.ByteString,
    module Data.Map,
    module Data.Maybe,
    module Data.Text,
    module Data.Validity,
    module GHC.Generics,
    module System.IO,
    (<$$>),
    toBinary,
  )
where

import Control.Lens (at, (.~), (?~), (^.), _1, _2)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bits (Bits (complement, shift, xor, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.Generics.Labels ()
import Data.List (unfoldr, foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Validity
  ( Validation (Validation, unValidation),
    ValidationChain (Location, Violated),
    Validity (validate),
  )
import Data.Validity.ByteString ()
import GHC.Generics (Generic)
import System.IO (FilePath)
import Prelude hiding (Word)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap f <$> x

toBinary :: Int -> Integer -> String
toBinary size input =
  let list = unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 2, x `div` 2)) input
      val =
        foldl'
          (\acc -> (++ acc) . show)
          ""
          list
   in replicate (size - length val) '0' ++ val
