{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.EndToEndSpec ( spec ) where


import qualified Data.ByteString.Char8         as BC

import           TinyRAM.EntryPoint            (handleCommand, readProgramFile)
import           TinyRAM.ExecuteProgram        (executeProgram)
import           TinyRAM.Spec.Prelude
import           TinyRAM.Types.Command         (Command (..))
import           TinyRAM.Types.InputTape       (InputTape (..))
import           TinyRAM.Types.Params          (Params (..))
import           TinyRAM.Types.Program         (Program (..))
import           TinyRAM.Types.ProgramFilePath (ProgramFilePath (..))


spec :: Spec
spec = describe "TinyRAM end to end" $ do
  simpleTestCase


simpleTestCase :: Spec
simpleTestCase =
  before (handleCommand (CommandParse (ProgramFilePath "examples/simple.s") objectFilePath)) $
    it "answers 7" $ do
      program <- Program . BC.concat . BC.lines . unProgram <$> readProgramFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape []) (InputTape [])
      answer `shouldBe` Right 7
  where
    objectFilePath = ProgramFilePath "examples/simple.o"
