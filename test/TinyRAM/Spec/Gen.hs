{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module TinyRAM.Spec.Gen
  ( genUnsignedInteger
  ) where


import Data.GenValidity.ByteString ()

import TinyRAM.Spec.Prelude
import TinyRAM.Types.Sign (Sign)
import TinyRAM.Types.WordSize (WordSize (..))


instance GenValid WordSize where
  genValid = WordSize . (8*) <$> choose (1, 32)
  shrinkValid = shrinkValidStructurally


instance GenValid Sign where
  genValid = elements [-1, 1]
  shrinkValid = shrinkValidStructurally


genUnsignedInteger :: WordSize -> Gen Integer
genUnsignedInteger (WordSize ws) = choose (0, 2 ^ ws - 1)
