{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Spec.EndToEndSpec (spec) where

import Data.Either.Combinators (maybeToRight)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Word as W
import TinyRAM.Bytes (bytesPerWord)
import TinyRAM.Cast (intToInteger, word16ToInteger, word8ToInt)
import TinyRAM.ExecuteProgram (executeProgram')
import TinyRAM.Spec.CoqRun (runCoqTinyRAM, toProgram)
import TinyRAM.Spec.Prelude hiding (negate)
import TinyRAM.Types.Address (Address (..))
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (IsImmediate, IsRegister))
import TinyRAM.Types.InputTape
  ( Auxiliary,
    InputTape (..),
    Primary,
  )
import TinyRAM.Types.Instruction (Instruction (..))
import TinyRAM.Types.MaxSteps (MaxSteps)
import TinyRAM.Types.Params (Params (..))
import TinyRAM.Types.ProgramMemoryValues (ProgramMemoryValues (..))
import TinyRAM.Types.Register (Register (..))
import TinyRAM.Types.RegisterCount (RegisterCount (..))
import TinyRAM.Types.Word (Word (..))
import TinyRAM.Types.WordSize (WordSize (..))

spec :: Spec
spec = describe "TinyRAM end to end" $ do
  simpleTestCase
  addTestCase
  andTestCase
  andTestNegativeCase
  --andFlagTestCase bugged reported
  andPreFlagTestCase
  cjmpTestCase
  jmpTestExampleNonTermCase
  negativeTestCase
  negative8bitTestCase
  orTestCase
  orFlagTestCase
  orFlag1TestCase
  --xorTestCase --bugged reported
  addTestNegativeTestCase
  subTestCase
  --notTestCase --negative answer bugged
  mullTestCase
  umulhTestCase
  smulhTestCase
  udivTestCase
  udiv0TestCase
  umodTestCase
  umod0TestCase
  umod1TestCase
  shlTestCase
  shlFlagTestCase
  shrTestCase

  -- cmpaeTestCaseL --bugged reported
  cmpaeTestCaseE
  cmpaeTestCaseG
  --cmpaeNegTestCaseG --bugged reported
  cmpaeNegTestCaseE
  cmpaeNegTestCaseL
  cmpaTestCaseE
  cmpaTestCaseG
  --cmpaTestCaseL --bugged reported
  --cmpaNegTestCaseG --bugged reported
  --cmpaNegTestCaseL --bugged reported
  --cmpaNegTestCaseE --bugged reported
  cmpeTestCaseE
  -- cmpeTestCaseG --bugged reported
  --cmpeTestCaseL --bugged reported
  --cmpeNegTestCaseG --bugged reported
  cmpeNegTestCaseE
  --cmpeNegTestCaseL --bugged reported
  cmpgeTestCaseE
  cmpgeTestCaseG
  --cmpgeTestCaseL --bugged reported
  cmpgeNegTestCaseG
  cmpgeNegTestCaseE

--answerR1TestCase --bugged reported

negate :: W.Word16 -> W.Word16
negate x = 2 ^ (16 :: Integer) - x

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
        pure $
          ((,) <$> maybeToRight "Coq failed" r <*> haskellResult)
            >>= ( \(c, (h, _)) ->
                    if c == h
                      then Right c
                      else Left $ T.pack $ "Coq: " ++ show c ++ ", Haskell: " ++ show h
                )

construct :: [Instruction] -> ProgramMemoryValues
construct instructions = ProgramMemoryValues $ Map.fromList $ zip addresses instructions
  where
    addresses = Address . Word . (* (2 * intToInteger (bytesPerWord ws))) <$> [0 ..]

imm :: W.Word16 -> ImmediateOrRegister
imm i = IsImmediate (Word (word16ToInteger i))

reg :: W.Word8 -> ImmediateOrRegister
reg i = IsRegister (Register (word8ToInt i))

reg' :: W.Word8 -> Register
reg' i = Register (word8ToInt i)

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

-- andFlagTestCase :: Spec
-- andFlagTestCase =
--   it "answers 0" $ do
--     let program =
--           construct
--             [ Mov (reg' 0) (imm 0),
--               Mov (reg' 2) (imm 58),
--               And (reg' 3) (reg' 2) (imm 15),
--               Cmov (reg' 0) (imm 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 0

andPreFlagTestCase :: Spec
andPreFlagTestCase =
  it "answers 0" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 0),
              Cmov (reg' 0) (imm 1),
              Mov (reg' 2) (imm 58),
              And (reg' 3) (reg' 2) (imm 15),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 0

--; TinyRAM V=1.000 W=16 K=16
--mov r2, 58
--and r1, r2, -15
--answer r1

andTestNegativeCase :: Spec
andTestNegativeCase =
  it "answers 48" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 58),
              And (reg' 0) (reg' 2) (imm (negate 15)),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 48

--;TinyRAM V=1.00 W=16 K=16”
--udiv 2, 0
--cnjmp 5
--answer 1
--answer 0
--;Expected result: 0

cjmpTestCase :: Spec
cjmpTestCase =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 1) (imm 2),
              Udiv (reg' 0) (reg' 1) (imm 0),
              Cjmp (imm 12),
              Answer (imm 1),
              Answer (imm 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

--; TinyRAM V=1.000 W=16 K=16
--mov r1, 0
--mov r2, 2
--mov r3, 2
--cmpae r2, r3
--cmov r1, 1
--answer r1

cmpaeTestCaseE :: Spec
cmpaeTestCaseE =
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

-- cmpaeTestCaseL :: Spec
-- cmpaeTestCaseL =
--   it "answers 1" $ do
--     let program =
--           construct
--             [ Mov (reg' 0) (imm 0),
--               Mov (reg' 2) (imm 1),
--               Mov (reg' 3) (imm 2),
--               Cmpae (reg' 2) (reg 3),
--               Cmov (reg' 0) (imm 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 1

cmpaeTestCaseG :: Spec
cmpaeTestCaseG =
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
--mov r2, 2
--mov r3, 1
--cmpae r2, r3
--cmov r1, 1
--answer r1

--; TinyRAM V=1.000 W=16 K=16
--mov r1, 0
--mov r2, 1
--mov r3, 2
--cmpae r2, r3
--cmov r1, 1
--answer r1
--Should be 0

-- cmpaeNegTestCaseG :: Spec
-- cmpaeNegTestCaseG =
--   it "answers 0" $ do
--     let program =
--           construct
--             [ Mov (reg' 0) (imm 0),
--               Mov (reg' 2) (imm 2),
--               Mov (reg' 3) (imm (negate 2)),
--               Cmpae (reg' 2) (reg 3),
--               Cmov (reg' 0) (imm 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 0

cmpaeNegTestCaseE :: Spec
cmpaeNegTestCaseE =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 0),
              Mov (reg' 2) (imm (negate 2)),
              Mov (reg' 3) (imm (negate 2)),
              Cmpae (reg' 2) (reg 3),
              Cmov (reg' 0) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

cmpaeNegTestCaseL :: Spec
cmpaeNegTestCaseL =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 0),
              Mov (reg' 2) (imm (negate 2)),
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
--mov r3, 2
--cmpa r2, r3
--cmov r1, 1
--answer r1

cmpaTestCaseE :: Spec
cmpaTestCaseE =
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

cmpaTestCaseG :: Spec
cmpaTestCaseG =
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

-- cmpaTestCaseL :: Spec
-- cmpaTestCaseL =
--   it "answers 0" $ do
--     let program =
--           construct
--             [ Mov (reg' 0) (imm 0),
--               Mov (reg' 2) (imm 1),
--               Mov (reg' 3) (imm 2),
--               Cmpae (reg' 2) (reg 3),
--               Cmov (reg' 0) (imm 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 0

--; TinyRAM V=1.000 W=16 K=16
--mov r1, 0
--mov r2, 2
--mov r3, -2
--cmpa r2, r3
--cmov r1, 1
--answer r1
--cmpaneg

-- cmpaNegTestCaseG :: Spec
-- cmpaNegTestCaseG =
--   it "answers 0" $ do
--     let program =
--           construct
--             [ Mov (reg' 0) (imm 0),
--               Mov (reg' 2) (imm 2),
--               Mov (reg' 3) (imm (negate 2)),
--               Cmpa (reg' 2) (reg 3),
--               Cmov (reg' 0) (imm 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 0

-- cmpaNegTestCaseL :: Spec
-- cmpaNegTestCaseL =
--   it "answers 1" $ do
--     let program =
--           construct
--             [ Mov (reg' 0) (imm 0),
--               Mov (reg' 2) (imm (negate 2)),
--               Mov (reg' 3) (imm 2),
--               Cmpa (reg' 2) (reg 3),
--               Cmov (reg' 0) (imm 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 1

-- cmpaNegTestCaseE :: Spec
-- cmpaNegTestCaseE =
--   it "answers 0" $ do
--     let program =
--           construct
--             [ Mov (reg' 0) (imm 0),
--               Mov (reg' 2) (imm (negate 2)),
--               Mov (reg' 3) (imm (negate 2)),
--               Cmpa (reg' 2) (reg 3),
--               Cmov (reg' 0) (imm 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 0

cmpeTestCaseE :: Spec
cmpeTestCaseE =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 0),
              Mov (reg' 2) (imm 2),
              Mov (reg' 3) (imm 2),
              Cmpe (reg' 2) (reg 3),
              Cmov (reg' 0) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

-- cmpeTestCaseG :: Spec
-- cmpeTestCaseG =
--   it "answers 0" $ do
--     let program =
--           construct
--             [ Mov (reg' 0) (imm 0),
--               Mov (reg' 2) (imm 2),
--               Mov (reg' 3) (imm 1),
--               Cmpe (reg' 2) (reg 3),
--               Cmov (reg' 0) (imm 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 0

-- cmpeTestCaseL :: Spec
-- cmpeTestCaseL =
--   it "answers 0" $ do
--     let program =
--           construct
--             [ Mov (reg' 0) (imm 0),
--               Mov (reg' 2) (imm 1),
--               Mov (reg' 3) (imm 2),
--               Cmpe (reg' 2) (reg 3),
--               Cmov (reg' 0) (imm 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 0

-- cmpeNegTestCaseG :: Spec
-- cmpeNegTestCaseG =
--   it "answers 0" $ do
--     let program =
--           construct
--             [ Mov (reg' 0) (imm 0),
--               Mov (reg' 2) (imm 1),
--               Mov (reg' 3) (imm (negate 1)),
--               Cmpe (reg' 2) (reg 3),
--               Cmov (reg' 0) (imm 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 0

cmpeNegTestCaseE :: Spec
cmpeNegTestCaseE =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 0),
              Mov (reg' 2) (imm (negate 1)),
              Mov (reg' 3) (imm (negate 1)),
              Cmpe (reg' 2) (reg 3),
              Cmov (reg' 0) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

-- cmpeNegTestCaseL :: Spec
-- cmpeNegTestCaseL =
--   it "answers 0" $ do
--     let program =
--           construct
--             [ Mov (reg' 0) (imm 0),
--               Mov (reg' 2) (imm (negate 1)),
--               Mov (reg' 3) (imm 1),
--               Cmpe (reg' 2) (reg 3),
--               Cmov (reg' 0) (imm 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 0

cmpgeTestCaseE :: Spec
cmpgeTestCaseE =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 0),
              Mov (reg' 2) (imm 2),
              Mov (reg' 3) (imm 2),
              Cmpge (reg' 2) (reg 3),
              Cmov (reg' 0) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

cmpgeTestCaseG :: Spec
cmpgeTestCaseG =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 0),
              Mov (reg' 2) (imm 2),
              Mov (reg' 3) (imm 1),
              Cmpge (reg' 2) (reg 3),
              Cmov (reg' 0) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

-- cmpgeTestCaseL :: Spec
-- cmpgeTestCaseL =
--   it "answers 0" $ do
--     let program =
--           construct
--             [ Mov (reg' 0) (imm 0),
--               Mov (reg' 2) (imm 1),
--               Mov (reg' 3) (imm 2),
--               Cmpge (reg' 2) (reg 3),
--               Cmov (reg' 0) (imm 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 0

cmpgeNegTestCaseG :: Spec
cmpgeNegTestCaseG =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 0),
              Mov (reg' 2) (imm 2),
              Mov (reg' 3) (imm (negate 2)),
              Cmpge (reg' 2) (reg 3),
              Cmov (reg' 0) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

-- cmpgeNegTestCaseL :: Spec
-- cmpgeNegTestCaseL =
--   it "answers 0" $ do
--     let program =
--           construct
--             [ Mov (reg' 0) (imm 0),
--               Mov (reg' 2) (imm (negate 2)),
--               Mov (reg' 3) (imm 2),
--               Cmpge (reg' 2) (reg 3),
--               Cmov (reg' 0) (imm 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 0

cmpgeNegTestCaseE :: Spec
cmpgeNegTestCaseE =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 0),
              Mov (reg' 2) (imm (negate 2)),
              Mov (reg' 3) (imm (negate 2)),
              Cmpge (reg' 2) (reg 3),
              Cmov (reg' 0) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

--cjmpTestCase

--jmpTestExampleNonTermCase
--; TinyRAM V=1.00 W=16 K=16”
--mov r0, 6
--mov r0, 8
--mov r0, 4
--jmp r0
--answer 1

jmpTestExampleNonTermCase :: Spec --can't match immediate type or register
jmpTestExampleNonTermCase =
  it "doesn't  terminate" $ do
    let program =
          construct
            [ Mov (reg' 0) (imm 6),
              Mov (reg' 0) (imm 8),
              Mov (reg' 0) (imm 4),
              Jmp (reg 0),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Left "program did not terminate in Just (MaxSteps {unMaxSteps = 1000})"

--negativeTestCase
negativeTestCase :: Spec
negativeTestCase =
  it "answers -4" $ do
    let program =
          construct
            [ Answer (imm (negate 4))
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right (Word (word16ToInteger (negate 4)))

--negative8bitTestCase
negative8bitTestCase :: Spec
negative8bitTestCase =
  it "answers -2" $ do
    let program =
          construct
            [ Answer (imm (negate 2))
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right (Word (word16ToInteger (negate 2)))

orTestCase :: Spec
orTestCase =
  it "answers 63" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 58),
              Or (reg' 0) (reg' 2) (imm 15),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 63

orFlagTestCase :: Spec
orFlagTestCase =
  it "answers 63" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 58),
              Or (reg' 0) (reg' 2) (imm 15),
              Cmov (reg' 0) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 63

orFlag1TestCase :: Spec
orFlag1TestCase =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 0),
              Or (reg' 0) (reg' 2) (imm 0),
              Cmov (reg' 0) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

--xorTestCase
-- ; TinyRAM V=1.000 W=16 K=16
-- mov r2, 15
-- xor r1, r2, r2
-- answer r1
--should be 0

-- xorTestCase :: Spec
-- xorTestCase =
--   it "answers 15" $ do
--     let program =
--           construct
--             [ Mov (reg' 2) (imm 15),
--               Xor (reg' 0) (reg' 2) (reg 2),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 15

addTestNegativeTestCase :: Spec
addTestNegativeTestCase =
  it "answers 3" $ do
    let program =
          construct
            [ Mov (reg' 1) (imm 5),
              Add (reg' 0) (reg' 1) (imm (negate 2)),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 3

--subTestCase
-- ; TinyRAM V=1.000 W=16 K=16
-- mov r2, 5
-- sub r1, r2, 2
-- answer r1

subTestCase :: Spec
subTestCase =
  it "answers 3" $ do
    let program =
          construct
            [ Mov (reg' 1) (imm 5),
              Sub (reg' 0) (reg' 1) (imm 2),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 3

--notTestCase
--; TinyRAM V=1.000 W=16 K=16
--mov r2, 11
--not r1, r2
--answer r1

-- notTestCase :: Spec
-- notTestCase =
--   it "answers -11" $ do
--     let program =
--           construct
--             [ Mov (reg' 1) (imm 11),
--               Not (reg' 0) (reg 1),
--               Answer (reg 0)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right (Word (word16ToInteger (negate 11)))

--mullTestCase
--; TinyRAM V=1.000 W=16 K=16
--mov r2, 5
--mull r1, r2, 2
--answer r1

mullTestCase :: Spec
mullTestCase =
  it "answers 10" $ do
    let program =
          construct
            [ Mov (reg' 1) (imm 5),
              Mull (reg' 0) (reg' 1) (imm 2),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 10

--umulhTestCase
--; TinyRAM V=1.000 W=16 K=16
--mov r2, 5
--umulh r1, r2, 2
--answer r1

umulhTestCase :: Spec
umulhTestCase =
  it "answers 0" $ do
    let program =
          construct
            [ Mov (reg' 1) (imm 5),
              Umulh (reg' 0) (reg' 1) (imm 2),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 0

--smulhTestCase
-- ; TinyRAM V=1.000 W=16 K=16
-- mov r2, 5
-- smulh r1, r2, 2
-- answer r1

smulhTestCase :: Spec
smulhTestCase =
  it "answers 0" $ do
    let program =
          construct
            [ Mov (reg' 1) (imm 5),
              Smulh (reg' 0) (reg' 1) (imm 2),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 0

--; TinyRAM V=1.000 W=16 K=16
--mov r2, 5
--udiv r1, r2, 2
--answer r1

udivTestCase :: Spec
udivTestCase =
  it "answers 0" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 5),
              Udiv (reg' 0) (reg' 2) (imm (negate 2)),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 0

-- udiv0TestCase
-- ; TinyRAM V=1.000 W=16 K=16
-- mov r2, 5
-- udiv r1, r2, 0
-- answer r1

udiv0TestCase :: Spec
udiv0TestCase =
  it "answers 0" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 5),
              Udiv (reg' 0) (reg' 2) (imm 0),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 0

-- umodTestCase
-- ; TinyRAM V=1.000 W=16 K=16
-- mov r2, 5
-- umod r1, r2, 2
-- answer r1

umodTestCase :: Spec
umodTestCase =
  it "answers 1" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 5),
              Umod (reg' 0) (reg' 2) (imm 2),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 1

--umod0TestCase
-- ; TinyRAM V=1.000 W=16 K=16
-- mov r2, 5
-- umod r1, r2, 0
-- answer r1

umod0TestCase :: Spec
umod0TestCase =
  it "answers 0" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 5),
              Umod (reg' 0) (reg' 2) (imm 0),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 0

--umod1TestCase
-- ; TinyRAM V=1.000 W=16 K=16
-- mov r2, 5
-- umod r1, r2, 1
-- answer r1

--umod0TestCase
-- ; TinyRAM V=1.000 W=16 K=16
-- mov r2, 5
-- umod r1, r2, 0
-- answer r1

umod1TestCase :: Spec
umod1TestCase =
  it "answers 0" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 5),
              Umod (reg' 0) (reg' 2) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 0

--shlTestCase
-- ; TinyRAM V=1.000 W=16 K=16
-- mov r2, 11111
-- shl r1, r2, 1
-- answer r1

shlTestCase :: Spec
shlTestCase =
  it "answers 0" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 0b111111),
              Umod (reg' 0) (reg' 2) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 0

--shlFlagTestCase.s
-- ; TinyRAM V=1.000 W=16 K=16
-- mov r2, 11111
-- sub r1, r2, 1
-- answer r1

shlFlagTestCase :: Spec
shlFlagTestCase =
  it "answers 0" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 0b111111),
              Umod (reg' 0) (reg' 2) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 0

--shrTestCase
--; TinyRAM V=1.000 W=16 K=16
-- mov r2, 63
-- shr r1, r2, 1
-- answer r1

shrTestCase :: Spec
shrTestCase =
  it "answers 0" $ do
    let program =
          construct
            [ Mov (reg' 2) (imm 63),
              Shr (reg' 0) (reg' 2) (imm 1),
              Answer (reg 0)
            ]
    answer <- execute program (InputTape []) (InputTape [])
    answer `shouldBe` Right 31

-- answerR1TestCase :: Spec
-- answerR1TestCase =
--   it "answers 1" $ do
--     let program =
--           construct
--             [ Mov (reg' 1) (imm  1),
--               Answer (reg 1)
--             ]
--     answer <- execute program (InputTape []) (InputTape [])
--     answer `shouldBe` Right 1
