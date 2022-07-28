{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.CoqTinyRAMSpec
  ( spec
  ) where


import           Control.Monad.Trans.Except        (runExceptT)
import           Control.Monad.Trans.State         (StateT (runStateT))
import           Data.ByteString                   (pack)
import           Data.Functor.Identity             (runIdentity)

import qualified Data.Map                          as Map


import           Control.Monad
import           TinyRAM.Bytes                     (bytesPerWord)
import           TinyRAM.Run                       (run)
import           TinyRAM.Spec.CoqRun               (byteToBitString,
                                                    bytesToBitString,
                                                    runCoqTinyRAM, toProgram)
import           TinyRAM.Spec.Gen                  (genProgramMemoryValues)
import           TinyRAM.Spec.Prelude
import           TinyRAM.Types.Address             (Address (..))
import           TinyRAM.Types.Flag                (Flag (..))
import           TinyRAM.Types.HasMachineState     (Error)
import           TinyRAM.Types.InputTape
import           TinyRAM.Types.MachineState
import           TinyRAM.Types.MaxSteps            (MaxSteps (..))
import           TinyRAM.Types.MemoryValues        (MemoryValues (..))
import           TinyRAM.Types.Params              (Params (..))
import           TinyRAM.Types.Program             (Program (..))
import           TinyRAM.Types.ProgramCounter      (ProgramCounter (..))
import           TinyRAM.Types.ProgramMemoryValues
import           TinyRAM.Types.Register            (Register (..))
import           TinyRAM.Types.RegisterCount       (RegisterCount (..))
import           TinyRAM.Types.RegisterValues      (RegisterValues (..))
import           TinyRAM.Types.TinyRAMT            (TinyRAMT (..))
import           TinyRAM.Types.Word                (Word (..))
import           TinyRAM.Types.WordSize            (WordSize (..))


spec :: Spec
spec = do
  byteToBitStringSpec
  bytesToBitStringSpec
  coqTinyRAMSpec


coqTinyRAMSpec :: Spec
coqTinyRAMSpec =
  describe "coq-tinyram" $ do
    coqTinyRAMSmokeTest
    answerTest
    answerRegisterTest
    readFromPrimaryTapeTest
    readFromSecondaryTapeTest
    emptyReadTest
    invalidReadTest
    programCounterPastEndTest
    generatedTests


coqTinyRAMSmokeTest :: Spec
coqTinyRAMSmokeTest =
  it "[COQ-TINYRAM-1] passes a smoke test" $ do
    result <- runCoqTinyRAM (Program "\xFC\0\0\0") (InputTape []) (InputTape []) (MaxSteps 6)
    result `shouldBe` (Just 0)
    return ()


answerTest :: Spec
answerTest =
  it "[COQ-TINYRAM-2] answers 4" $ do
    result <- runCoqTinyRAM (Program "\xFC\0\0\x04") (InputTape []) (InputTape []) (MaxSteps 6)
    result `shouldBe` (Just 4)


answerRegisterTest :: Spec
answerRegisterTest =
  it "[COQ-TINYRAM-3] answers 0" $ do
    result <- runCoqTinyRAM (Program "\xF8\0\0\x02") (InputTape []) (InputTape []) (MaxSteps 5)
    result `shouldBe` (Just 0)


readFromPrimaryTapeTest :: Spec
readFromPrimaryTapeTest =
  it "[COQ-TINYRAM-4] reads from the primary input tape and provides output" $ do
    result <- runCoqTinyRAM (Program "\xF4\0\0\0\xF8\0\0\0") (InputTape [2]) (InputTape []) (MaxSteps 6)
    result `shouldBe` (Just 2)


readFromSecondaryTapeTest :: Spec
readFromSecondaryTapeTest =
  it "[COQ-TINYRAM-5] reads from the secondary input tape and provides output" $ do
    result <- runCoqTinyRAM (Program "\xF4\0\0\x01\xF8\0\0\0") (InputTape []) (InputTape [2]) (MaxSteps 6)
    result `shouldBe` (Just 2)


emptyReadTest :: Spec
emptyReadTest =
  it "[COQ-TINYRAM-6] does not crash when reading from an empty tape" $ do
    result <- runCoqTinyRAM (Program "\xF4\0\0\0\xF8\0\0\0") (InputTape []) (InputTape []) (MaxSteps 6)
    result `shouldBe` (Just 0)


invalidReadTest :: Spec
invalidReadTest =
  it "[COQ-TINYRAM-7] does not crash when reading from a non-existent tape with random crud in the unused part of the instruction" $ do
    result <- runCoqTinyRAM (Program "\xF4\x44\x44\x44\xF8\0\0\0") (InputTape []) (InputTape []) (MaxSteps 6)
    result `shouldBe` (Just 0)


programCounterPastEndTest :: Spec
programCounterPastEndTest =
  it "[COQ-TINYRAM-PROPERTY] answers 1 if the program counter goes past the end of the program" $ do
    result <- runCoqTinyRAM (Program "\xD8\x00\xFF\xF8\xF8\0\0\0") (InputTape []) (InputTape []) (MaxSteps 6)
    result `shouldBe` (Just 1)


emptyRegisterValues :: RegisterCount -> RegisterValues
emptyRegisterValues rc = RegisterValues $ Map.fromList
  (take (unRegisterCount rc) (zip (Register <$> [0..]) (Word <$> repeat 0)))

emptyMemoryValues :: WordSize -> MemoryValues
emptyMemoryValues ws = MemoryValues $ Map.fromList
  (take (2 ^ unWordSize ws) (zip addresses (Word <$> repeat 0)))
  where
    addresses = Address . Word . (* fromIntegral (bytesPerWord ws)) <$> [0..]

createMachineState :: WordSize  -> RegisterCount -> ProgramMemoryValues -> MachineState
createMachineState ws rc v =  MachineState (ProgramCounter 0)
              (emptyRegisterValues rc)
              (Flag 0)
              (emptyMemoryValues ws)
              v
              (InputTape [])
              (InputTape [])

execute :: MaxSteps
 -> Params
 -> MachineState
 -> Either Error (Maybe Word, (Params, MachineState))
execute maxSteps ps s = runIdentity . runExceptT . runStateT (unTinyRAMT (run (Just maxSteps))) $ (ps, s)


generatedTests :: Spec
generatedTests =
  it "produces the same result as the Haskell TinyRAM emulator" $
    forAll (genProgramMemoryValues ws rc) $ \programMemoryValues ->
      forAll (MaxSteps <$> choose (1,1000)) $ \maxSteps -> do
        let s = createMachineState ws rc programMemoryValues
            prog = toProgram ws rc (s ^. #programMemoryValues)
        coqResult <- runCoqTinyRAM prog (s ^. #primaryInput) (s ^. #auxiliaryInput) maxSteps
        -- print coqResult
        let hsResult = execute maxSteps ps s
        -- print hsResult
        case (coqResult, hsResult) of
          (_, Left _)       -> return () -- TODO more granularly compare error results
          (x, Right (y, _)) -> do
            x `shouldBe` y
  where
    ws = WordSize 16
    rc = RegisterCount 4
    ps = Params ws rc


bytesToBitStringSpec :: Spec
bytesToBitStringSpec =
  describe "bytesToBitString" $
    it "works as expected on an example" $
      bytesToBitString (pack [0, 13, 255, 64])
        `shouldBe` "00000000000011011111111101000000"


byteToBitStringSpec :: Spec
byteToBitStringSpec =
  describe "byteToBitString" $ do
    it "works as expected on 0" $
      byteToBitString 0 `shouldBe` "00000000"
    it "works as expected on 1" $
      byteToBitString 1 `shouldBe` "00000001"
    it "works as expected on 2" $
      byteToBitString 2 `shouldBe` "00000010"
    it "works as expected on 5" $
      byteToBitString 5 `shouldBe` "00000101"
    it "works as expected on 254" $
      byteToBitString 254 `shouldBe` "11111110"
    it "works as expected on 255" $
      byteToBitString 255 `shouldBe` "11111111"
