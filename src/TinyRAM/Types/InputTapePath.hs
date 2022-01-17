{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.InputTapePath
  ( InputTapePath (..)
  , Primary
  , Auxiliary
  ) where


import TinyRAM.Prelude
import TinyRAM.Types.InputTape (Primary, Auxiliary)


newtype InputTapePath a = InputTapePath { unInputTapePath :: FilePath }
  deriving Generic
