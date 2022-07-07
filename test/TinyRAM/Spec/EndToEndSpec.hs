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
  xorTestCase
  addTestCase
  subTestCase
  notTestCase
  mullTestCase
  umulhTestCase
  smulhTestCase
  udivTestCase
  umodTestCase
  shlTestCase
  shrTestCase
  cmpaeLessTestCase
  cmpaLessTestCase
  cmpeLessTestCase
  cmpgeLessTestCase
  cmpgLessTestCase
  cmpaeEqualTestCase
  cmpaEqualTestCase
  cmpeEqualTestCase
  cmpgeEqualTestCase
  cmpgEqualTestCase
  cmpaeGreaterTestCase
  cmpaGreaterTestCase
  cmpeGreaterTestCase
  cmpgeGreaterTestCase
  cmpgGreaterTestCase

 

  umod0Test
  umod1Test
  addTestNegative
  cmpaeNegTest
  cmpaNegTest
  cmpeNegTest
  cmpgeNegTest
  cmpgNegTest




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

xorTestCase :: Spec
xorTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/xorTest.s") objectFilePath)) $
    it "The XOR operation should result in 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 0
  where 
    objectFilePath = ObjectFilePath "examples/xorTest.o"

notTestCase :: Spec
notTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/notTest.s") objectFilePath)) $
    it "The NOT operation should result in 65524." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 65524
  where 
    objectFilePath = ObjectFilePath "examples/notTest.o"

addTestCase :: Spec
addTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/addTest.s") objectFilePath)) $
    it "The ADD operation should result in 7." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 7
  where 
    objectFilePath = ObjectFilePath "examples/addTest.o"

subTestCase :: Spec
subTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/subTest.s") objectFilePath)) $
    it "The SUB operation should result in 3." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 3
  where 
    objectFilePath = ObjectFilePath "examples/subTest.o"

mullTestCase :: Spec
mullTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/mullTest.s") objectFilePath)) $
    it "The MULL operation should result in 10." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 10
  where 
    objectFilePath = ObjectFilePath "examples/mullTest.o"

umulhTestCase :: Spec
umulhTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/umulhTest.s") objectFilePath)) $
    it "The UMULH operation should result in 10." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 0
  where 
    objectFilePath = ObjectFilePath "examples/umulhTest.o"

smulhTestCase :: Spec
smulhTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/smulhTest.s") objectFilePath)) $
    it "The SMULH operation should result in 10." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 0
  where 
    objectFilePath = ObjectFilePath "examples/smulhTest.o"

udivTestCase :: Spec
udivTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/udivTest.s") objectFilePath)) $
    it "The UDIV operation should result in 2." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 2
  where 
    objectFilePath = ObjectFilePath "examples/udivTest.o"

umodTestCase :: Spec
umodTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/umodTest.s") objectFilePath)) $
    it "The UMOD operation should result in 1." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 1
  where 
    objectFilePath = ObjectFilePath "examples/umodTest.o"

shlTestCase :: Spec
shlTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/shlTest.s") objectFilePath)) $
    it "The SHL operation should result in 11110." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (11110)
  where 
    objectFilePath = ObjectFilePath "examples/shlTest.o"

shrTestCase :: Spec
shrTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/shrTest.s") objectFilePath)) $
    it "The SHR operation should result in 01111." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 31
  where 
    objectFilePath = ObjectFilePath "examples/shrTest.o"

cmpaeLessTestCase :: Spec
cmpaeLessTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpaeLessTest.s") objectFilePath)) $
    it "The CMPAE operation should result in setting the flag to 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (0)
  where 
    objectFilePath = ObjectFilePath "examples/cmpaeLessTest.o"

cmpaLessTestCase :: Spec
cmpaLessTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpaLessTest.s") objectFilePath)) $
    it "The CMPA operation should result in setting the flag to 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (0)
  where 
    objectFilePath = ObjectFilePath "examples/cmpaLessTest.o"

cmpeLessTestCase :: Spec
cmpeLessTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpeLessTest.s") objectFilePath)) $
    it "The CMPE operation should result in setting the flag to 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (0)
  where 
    objectFilePath = ObjectFilePath "examples/cmpeLessTest.o"

cmpgeLessTestCase :: Spec
cmpgeLessTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpgeLessTest.s") objectFilePath)) $
    it "The CMPGE operation should result in setting the flag to 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (0)
  where 
    objectFilePath = ObjectFilePath "examples/cmpgeLessTest.o"

cmpgLessTestCase :: Spec
cmpgLessTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpgLessTest.s") objectFilePath)) $
    it "The CMPG operation should result in setting the flag to 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (0)
  where 
    objectFilePath = ObjectFilePath "examples/cmpgLessTest.o"

cmpaeEqualTestCase :: Spec
cmpaeEqualTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpaeEqualTest.s") objectFilePath)) $
    it "The CMPAE operation should result in setting the flag to 1." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (1)
  where 
    objectFilePath = ObjectFilePath "examples/cmpaeEqualTest.o"

cmpaEqualTestCase :: Spec
cmpaEqualTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpaEqualTest.s") objectFilePath)) $
    it "The CMPA operation should result in setting the flag to 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (0)
  where 
    objectFilePath = ObjectFilePath "examples/cmpaEqualTest.o"

cmpeEqualTestCase :: Spec
cmpeEqualTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpeEqualTest.s") objectFilePath)) $
    it "The CMPE operation should result in setting the flag to 1." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (1)
  where 
    objectFilePath = ObjectFilePath "examples/cmpeEqualTest.o"

cmpgeEqualTestCase :: Spec
cmpgeEqualTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpgeEqualTest.s") objectFilePath)) $
    it "The CMPGE operation should result in setting the flag to 1." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (1)
  where 
    objectFilePath = ObjectFilePath "examples/cmpgeEqualTest.o"

cmpgEqualTestCase :: Spec
cmpgEqualTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpgEqualTest.s") objectFilePath)) $
    it "The CMPG operation should result in setting the flag to 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (0)
  where 
    objectFilePath = ObjectFilePath "examples/cmpgEqualTest.o"

cmpaeGreaterTestCase :: Spec
cmpaeGreaterTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpaeGreaterTest.s") objectFilePath)) $
    it "The CMPAE operation should result in setting the flag to 1." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (1)
  where 
    objectFilePath = ObjectFilePath "examples/cmpaeGreaterTest.o"

cmpaGreaterTestCase :: Spec
cmpaGreaterTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpaGreaterTest.s") objectFilePath)) $
    it "The CMPA operation should result in setting the flag to 1." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (1)
  where 
    objectFilePath = ObjectFilePath "examples/cmpaGreaterTest.o"

cmpeGreaterTestCase :: Spec
cmpeGreaterTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpeGreaterTest.s") objectFilePath)) $
    it "The CMPE operation should result in setting the flag to 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (0)
  where 
    objectFilePath = ObjectFilePath "examples/cmpeGreaterTest.o"

cmpgeGreaterTestCase :: Spec
cmpgeGreaterTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpgeGreaterTest.s") objectFilePath)) $
    it "The CMPGE operation should result in setting the flag to 1." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (1)
  where 
    objectFilePath = ObjectFilePath "examples/cmpgeGreaterTest.o"

cmpgGreaterTestCase :: Spec
cmpgGreaterTestCase = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpgGreaterTest.s") objectFilePath)) $
    it "The CMPG operation should result in setting the flag to 1." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (1)
  where 
    objectFilePath = ObjectFilePath "examples/cmpgGreaterTest.o"



umod0Test :: Spec
umod0Test = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/umod0Test.s") objectFilePath)) $
    it "The UMOD 0 operation should result in 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 0
  where 
    objectFilePath = ObjectFilePath "examples/umod0Test.o"

umod1Test :: Spec
umod1Test = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/umod1Test.s") objectFilePath)) $
    it "The UMOD 1 operation should result in 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 0
  where 
    objectFilePath = ObjectFilePath "examples/umod1Test.o"



addTestNegative :: Spec
addTestNegative = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/addTestNegative.s") objectFilePath)) $
    it "The ADD operation should result in 3." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right 3
  where 
    objectFilePath = ObjectFilePath "examples/addTestNegative.o"

cmpaeNegTest :: Spec
cmpaeNegTest = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpaeNegTest.s") objectFilePath)) $
    it "The CMPAE operation should result in setting the flag to 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (0)
  where 
    objectFilePath = ObjectFilePath "examples/cmpaeNegTest.o"

cmpaNegTest :: Spec
cmpaNegTest = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpaNegTest.s") objectFilePath)) $
    it "The CMPA operation should result in setting the flag to 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (0)
  where 
    objectFilePath = ObjectFilePath "examples/cmpaNegTest.o"

cmpeNegTest :: Spec
cmpeNegTest = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpeNegTest.s") objectFilePath)) $
    it "The CMPE operation should result in setting the flag to 0." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (0)
  where 
    objectFilePath = ObjectFilePath "examples/cmpeNegTest.o"

cmpgeNegTest :: Spec
cmpgeNegTest = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpgeNegTest.s") objectFilePath)) $
    it "The CMPGE operation should result in setting the flag to 1." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (1)
  where 
    objectFilePath = ObjectFilePath "examples/cmpgeNegTest.o"

cmpgNegTest :: Spec
cmpgNegTest = 
  before (handleCommand (CommandParse (AssemblyFilePath "examples/cmpgNegTest.s") objectFilePath)) $
    it "The CMPG operation should result in setting the flag to 1." $ do
      program <- readObjectFile objectFilePath
      let answer = executeProgram (Params 16 16) (Just 1000) program (InputTape [1,2,3,4]) (InputTape [1,2,3])
      answer `shouldBe` Right (1)
  where 
    objectFilePath = ObjectFilePath "examples/cmpgNegTest.o"
