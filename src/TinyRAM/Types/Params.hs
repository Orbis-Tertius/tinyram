{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}


module TinyRAM.Types.Params ( Params (Params) ) where


import           TinyRAM.Prelude
import           TinyRAM.Types.RegisterCount (RegisterCount)
import           TinyRAM.Types.WordSize      (WordSize)


data Params =
  Params
  { wordSize      :: WordSize
  , registerCount :: RegisterCount
  }
  deriving (Eq, Ord, Read, Show, Generic)
