{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Spec.EndToEndSpec (spec) where

import Data.Either.Combinators (maybeToRight)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Word as W
import TinyRAM.Bytes (bytesPerWord)
import TinyRAM.ExecuteProgram (executeProgram')
import TinyRAM.Spec.CoqRun
import TinyRAM.Spec.Prelude
import TinyRAM.Types.Address
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (IsImmediate, IsRegister))
import TinyRAM.Types.InputTape
  ( Auxiliary,
    InputTape (..),
    Primary,
  )
import TinyRAM.Types.Instruction
import TinyRAM.Types.MaxSteps (MaxSteps)
import TinyRAM.Types.Params (Params (..))
import TinyRAM.Types.ProgramMemoryValues
import TinyRAM.Types.Register
import TinyRAM.Types.RegisterCount
import TinyRAM.Types.Word
import TinyRAM.Types.WordSize

spec :: Spec
spec = describe "TinyRAM end to end" $ do
  simpleTestCase
  addTestCase
  andTestCase
  --andTestNegativeCase
  --cjmpTestCase
  cmpaeEqualTestCase
  cmpaeGreaterTestCase
  --cmpaeLessTestCase
  cmpaEqualTestCase
  cmpaGreaterTestCase

--cmpaLessTestCase

ws :: WordSize
ws = 16

rc :: RegisterCount
rc = 16

params :: Params
params = Params ws rc

maxSteps :: MaxSteps
maxSteps = 1000

execute ::
  ProgramMemoryValues ->
  InputTape Primary ->
  InputTape Auxiliary ->
  IO (Either Text Word)
execute prog t1 t2 =
  let haskellResult = executeProgram' params (Just maxSteps) prog t1 t2
      coqResult = runCoqTinyRAM (toProgram ws rc prog) t1 t2 maxSteps
   in do
        r <- coqResult
        return $
          ((,) <$> maybeToRight "Coq failed" r <*> haskellResult)
            >>= ( \(c, h) ->
                    if c == h
                      then Right c
                      else Left $ T.pack $ "Coq: " ++ show c ++ ", Haskell: " ++ show h
                )

construct :: [Instruction] -> ProgramMemoryValues
construct instructions = ProgramMemoryValues $ Map.fromList $ zip addresses instructions
  where
    addresses = Address . Word . (* (2 * fromIntegral (bytesPerWord ws))) <$> [0 ..]

imm :: W.Word16 -> ImmediateOrRegister
imm i = IsImmediate (Word (fromIntegral i))

reg :: W.Word8 -> ImmediateOrRegister
reg i = IsRegister (Register (fromIntegral i))

reg' :: W.Word8 -> Register
reg' i = Register (fromIntegral i)

--When translating programs, answer register must be reg 0.
--Example: ", Answer (reg 0)"

simpleTestCase :: Spec
simpleTestCase =
  it "answers 7" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 7),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 7

--; TinyRAM V=1.000 W=16 K=16
--mov r2, 5
--add r1, r2, 2
--answer r1

addTestCase :: Spec
addTestCase =
  it "answers 7" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 5),
              Add (reg' 0) (reg' 2) (imm 2),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 7

--; TinyRAM V=1.000 W=16 K=16
--mov r2, 58
--and r1, r2, 15
--answer r1

andTestCase :: Spec
andTestCase =
  it "answers 10" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 58),
              And (reg' 0) (reg' 2) (imm 15),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 10

--; TinyRAM V=1.000 W=16 K=16
--mov r2, 58
--and r1, r2, -15
--answer r1

--andTestNegativeCase :: Spec
--andTestNegativeCase =
--it "answers 10" $  do
--let program = construct [
--Mov (reg' 2)(imm 58)
--, And (reg' 0) (reg' 2) (imm (-15))
--, Answer (reg 0)
--]
--answer <- execute program (InputTape []) (InputTape [])
--answer `shouldBe` Right 10

--breakWKconstraintTestCase :: Spec
--breakWKconstraintTestCase =
--it "answers " $  do
--let program = construct [
--Answer (imm 0)
--]

--;TinyRAM V=1.00 W=16 K=16”
--udiv 2, 0
--cnjmp 5
--answer 1
--answer 0
--;Expected result: 0

--cjmpTestCase :: Spec
--cjmpTestCase =
--it "answers 0" $ do
--let program = construct [
--Udiv (imm 2) (imm 0)
--, Cnjmp (imm 5)
--, Answer (imm 1)
--, Answer (imm 0)
--]
--answer <- execute program (InputTape []) (InputTape [])
--answer `shouldBe` Right 0

--; TinyRAM V=1.000 W=16 K=16
--mov r1, 0
--mov r2, 2
--mov r3, 2
--cmpae r2, r3
--cmov r1, 1
--answer r1

cmpaeEqualTestCase :: Spec
cmpaeEqualTestCase =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 0),
              Mov (reg' 2) (imm 2),
              Mov (reg' 3) (imm 2),
              Cmpae (reg' 2) (reg 3),
              Cmov (reg' 0) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

--; TinyRAM V=1.000 W=16 K=16
--mov r1, 0
--mov r2, 2
--mov r3, 1
--cmpae r2, r3
--cmov r1, 1
--answer r1

cmpaeGreaterTestCase :: Spec
cmpaeGreaterTestCase =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 0),
              Mov (reg' 2) (imm 2),
              Mov (reg' 3) (imm 1),
              Cmpae (reg' 2) (reg 3),
              Cmov (reg' 0) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

--; TinyRAM V=1.000 W=16 K=16
--mov r1, 0
--mov r2, 1
--mov r3, 2
--cmpae r2, r3
--cmov r1, 1
--answer r1
--Should be 0

--cmpaeLessTestCase :: Spec
--cmpaeLessTestCase =
--it "answers 0" $ do
--let program = construct [
--Mov (reg' 0) (imm 0)
--, Mov (reg' 2) (imm 1)
--, Mov (reg' 3) (imm 2)
--, Cmpae (reg' 2) (reg 3)
--, Cmov (reg' 0) (imm 1)
--, Answer (reg 0)
--]
--answer <- execute program (InputTape []) (InputTape [])
--answer `shouldBe` Left 0

--; TinyRAM V=1.000 W=16 K=16
--mov r1, 0
--mov r2, 2
--mov r3, 2
--cmpa r2, r3
--cmov r1, 1
--answer r1

cmpaEqualTestCase :: Spec
cmpaEqualTestCase =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 0),
              Mov (reg' 2) (imm 2),
              Mov (reg' 3) (imm 2),
              Cmpae (reg' 2) (reg 3),
              Cmov (reg' 0) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

--; TinyRAM V=1.000 W=16 K=16
--mov r1, 0
--mov r2, 2
--mov r3, 1
--cmpa r2, r3
--cmov r1, 1
--answer r1 Should be 1

cmpaGreaterTestCase :: Spec
cmpaGreaterTestCase =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 0),
              Mov (reg' 2) (imm 2),
              Mov (reg' 3) (imm 1),
              Cmpae (reg' 2) (reg 3),
              Cmov (reg' 0) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

--; TinyRAM V=1.000 W=16 K=16
--mov r1, 0
--mov r2, 1
--mov r3, 2
--cmpa r2, r3
--cmov r1, 1
--answer r1
--right 0

--cmpaLessTestCase :: Spec
--cmpaLessTestCase =
--it "answers 1" $ do
--let program = construct [
--Mov (reg' 0) (imm 0)
--, Mov (reg' 2) (imm 1)
--, Mov (reg' 3) (imm 2)
--, Cmpae (reg' 2) (reg 3)
--, Cmov (reg' 0) (imm 1)
--, Answer (reg 0)
--]
--answer <- execute program (InputTape []) (InputTape [])
--answer `shouldBe` Left 1

--; TinyRAM V=1.000 W=16 K=16
--mov r1, 0
--mov r2, 2
--mov r3, -2
--cmpa r2, r3
--cmov r1, 1
--answer r1
--cmpaneg
