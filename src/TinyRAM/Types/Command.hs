{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.Command ( Command (CommandRun) ) where


import TinyRAM.Prelude
import TinyRAM.Types.InputTapePath (InputTapePath, Primary, Auxiliary)
import TinyRAM.Types.MaxSteps (MaxSteps)
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.ProgramFilePath (ProgramFilePath)


data Command =
  CommandRun
  { params :: Params
  , maxSteps :: Maybe MaxSteps
  , programFilePath :: ProgramFilePath
  , primaryInputTapePath :: InputTapePath Primary
  , auxiliaryInputTapePath :: InputTapePath Auxiliary
  }
  deriving Generic
