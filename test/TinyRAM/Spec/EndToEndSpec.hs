{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.EndToEndSpec ( spec ) where



import           TinyRAM.EntryPoint            (handleCommand, readObjectFile)
import           TinyRAM.ExecuteProgram        (executeProgram)
import           TinyRAM.Spec.Prelude
import           TinyRAM.Types.Command         (Command (..))
import           TinyRAM.Types.InputTape       (InputTape (..))
import           TinyRAM.Types.Params          (Params (..))
import           TinyRAM.Types.ProgramFilePath (ObjectFilePath (..))
import           TinyRAM.Types.ProgramFilePath (AssemblyFilePath (..))

spec :: Spec
spec = describe "TinyRAM end to end" $ do
  simpleTestCase


simpleTestCase :: Spec
simpleTestCase =
  before (handleCommand (CommandParse (AssemblyFilePath "examples/simple.s") objectFilePath)) $
    it "answers 7" $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape []) (InputTape [])
      answer `shouldBe` Right 7
  where
    objectFilePath = ObjectFilePath "examples/simple.o"
