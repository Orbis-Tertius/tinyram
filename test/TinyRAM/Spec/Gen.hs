{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module TinyRAM.Spec.Gen
  (
  ) where


import Data.GenValidity.ByteString ()

import TinyRAM.Spec.Prelude
import TinyRAM.Types.WordSize (WordSize (..))


instance GenValid WordSize where
  genValid = WordSize . (8*) <$> choose (1, 32)
  shrinkValid = shrinkValidStructurally
