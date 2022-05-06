{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.EndToEndSpec ( spec ) where


import           TinyRAM.EntryPoint            (handleCommand,
                                                readObjectFile)
import           TinyRAM.ExecuteProgram        (executeProgram)
import           TinyRAM.Spec.Prelude
import           TinyRAM.Types.Command         (Command (..))
import           TinyRAM.Types.InputTape       (InputTape (..))
import           TinyRAM.Types.Params          (Params (..))
import           TinyRAM.Types.ProgramFilePath (AssemblyFilePath (..),
                                                ObjectFilePath (..))

spec :: Spec
spec = describe "TinyRAM end to end" $ do
  simpleTestCase
  nonExistentTapeTestCase
  negativeTestCase
  negative8bitTestCase
  breakWKconstraintTestCase
  andTestCase
  orTestCase
  notTestCase

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
  before (handleCommand (CommandParse (AssemblyFilePath "examples/breakWKconstraint.s") objectFilePath)) $
    it "answers 2, if changing the word size to 8 with 16 registers does not break the constraints." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 8 2) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (2)
  where 
    objectFilePath = ObjectFilePath "examples/breakWKconstraint.o"

andTestCase :: Spec
andTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/andTest.s") objectFilePath)) $
    it "answers 00001010, anding 00111010 with 0FG should clear the high order bits." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 10
  where 
    objectFilePath = ObjectFilePath "examples/andTest.o"

orTestCase :: Spec
orTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/orTest.s") objectFilePath)) $
    it "The OR operation should result in (63)." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (63)
  where 
    objectFilePath = ObjectFilePath "examples/orTest.o"

notTestCase :: Spec
notTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/notTest.s") objectFilePath)) $
    it "The NOT operation should result in 65524." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 65524
  where 
    objectFilePath = ObjectFilePath "examples/notTest.o"
