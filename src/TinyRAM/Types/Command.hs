{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.Command (Command (CommandRun)) where

import TinyRAM.Prelude
import TinyRAM.Types.InputTape (Auxiliary, Primary)
import TinyRAM.Types.InputTapePath (InputTapePath)
import TinyRAM.Types.MaxSteps (MaxSteps)
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.ProgramFilePath (ObjectFilePath)

data Command = CommandRun
  { params :: Params,
    maxSteps :: Maybe MaxSteps,
    objectFilePath :: ObjectFilePath,
    primaryInputTapePath :: InputTapePath Primary,
    auxiliaryInputTapePath :: InputTapePath Auxiliary
  }
  deriving stock (Generic)
