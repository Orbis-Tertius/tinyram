{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.Command ( Command (CommandRun, CommandParse) ) where


import           TinyRAM.Prelude
import           TinyRAM.Types.InputTapePath   (Auxiliary, InputTapePath,
                                                Primary)
import           TinyRAM.Types.MaxSteps        (MaxSteps)
import           TinyRAM.Types.Params          (Params)
import           TinyRAM.Types.ProgramFilePath (AssemblyFilePath,
                                                ObjectFilePath)


data Command =
  CommandRun
    { params                 :: Params
    , maxSteps               :: Maybe MaxSteps
    , objectFilePath         :: ObjectFilePath
    , primaryInputTapePath   :: InputTapePath Primary
    , auxiliaryInputTapePath :: InputTapePath Auxiliary
    }
  | CommandParse
    { assemblyFilePath :: AssemblyFilePath
    , outputFilePath   :: ObjectFilePath
    }
  deriving Generic
