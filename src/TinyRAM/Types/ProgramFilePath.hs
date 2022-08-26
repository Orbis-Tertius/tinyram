{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.ProgramFilePath
  ( AssemblyFilePath (..),
    ObjectFilePath (..),
  )
where

import TinyRAM.Prelude

newtype AssemblyFilePath = AssemblyFilePath {unAssemblyFilePath :: FilePath}
  deriving stock (Generic)

newtype ObjectFilePath = ObjectFilePath {unObjectFilePath :: FilePath}
  deriving stock (Generic)
