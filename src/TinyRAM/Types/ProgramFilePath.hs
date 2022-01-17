{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.ProgramFilePath ( ProgramFilePath (..) ) where


import TinyRAM.Prelude


newtype ProgramFilePath = ProgramFilePath { unProgramFilePath :: FilePath }
  deriving Generic
