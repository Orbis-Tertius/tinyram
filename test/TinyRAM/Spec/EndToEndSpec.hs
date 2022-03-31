{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.EndToEndSpec ( spec ) where


import Control.Monad.IO.Class (liftIO)

import           TinyRAM.EntryPoint            (handleCommand, readObjectFile, assemble)
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
  nonExistentTapeTestCase
  negativeTestCase
  negative8bitTestCase
  breakWKconstraintTestCase

simpleTestCase :: Spec
simpleTestCase =
  before (handleCommand (CommandParse (AssemblyFilePath "examples/simple.s") objectFilePath)) $
    it "answers 7" $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape []) (InputTape [])
      answer `shouldBe` Right 7
  where
    objectFilePath = ObjectFilePath "examples/simple.o"

nonExistentTapeTestCase :: Spec
nonExistentTapeTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/nonexistent-tape.s") objectFilePath)) $
    it "answers 0" $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 0
  where
    objectFilePath = ObjectFilePath "examples/nonexistent-tape.o"

negativeTestCase :: Spec
negativeTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/negative.s") objectFilePath)) $
    it "answers 65532, the two's complement of -4 for a 16 bit word" $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (65532)
  where 
    objectFilePath = ObjectFilePath "examples/negative.o"

negative8bitTestCase :: Spec
negative8bitTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/negative8bit.s") objectFilePath)) $
    it "answers 254, the two's complement of -2 for a 8 bit word" $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 8 8) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (254)
  where 
    objectFilePath = ObjectFilePath "examples/negative8bit.o"

breakWKconstraintTestCase :: Spec
breakWKconstraintTestCase = 
  it "fails to assemble when the WK constraint is not satisfied" $ do
    result <- liftIO $ assemble
      (AssemblyFilePath "examples/breakWKconstraint.s")
      (ObjectFilePath "examples/breakWKconstraint.o")
    result `shouldBe` (Left "The constraint 6 + 2*ceil(log_2(K)) <= W is not satisfied.")
