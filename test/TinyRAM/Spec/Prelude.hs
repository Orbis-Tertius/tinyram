{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Spec.Prelude
  ( module TinyRAM.Prelude
  , module Data.GenValidity
  , module Test.QuickCheck
  , module Test.Syd
  , module Test.Syd.Validity
  ) where


import Data.GenValidity (GenValid (..), genValidStructurally, shrinkValidStructurally)
import Test.QuickCheck (Gen, forAll, choose, elements, listOf, oneof)

import TinyRAM.Prelude
import Test.Syd (Spec, describe, it, shouldBe)
import Test.Syd.Validity (forAllValid)
