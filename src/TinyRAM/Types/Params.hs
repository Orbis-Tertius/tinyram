{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.Params (Params (Params)) where

import TinyRAM.Prelude
import TinyRAM.Types.RegisterCount (RegisterCount)
import TinyRAM.Types.WordSize (WordSize)

data Params = Params
  { wordSize :: WordSize,
    registerCount :: RegisterCount
  }
  deriving stock (Eq, Ord, Read, Show, Generic)
