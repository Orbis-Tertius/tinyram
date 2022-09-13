{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.InputTapePath
  ( InputTapePath (..),
  )
where

import TinyRAM.Prelude (FilePath, Generic)

newtype InputTapePath a = InputTapePath {unInputTapePath :: FilePath}
  deriving stock (Generic)
