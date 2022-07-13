{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.CoqTinyRAMSpec
  ( bytesToBitString
  , byteToBitString
  , spec
  ) where


import           Control.Monad                (when)
import           Control.Monad.Trans.Except   (runExceptT)
import           Control.Monad.Trans.State    (StateT (runStateT))
import           Data.Bits                    (testBit)
import           Data.ByteString              (pack, unpack)
import qualified Data.ByteString              as BS
import           Data.Functor.Identity        (runIdentity)
import           Data.List                    (isPrefixOf)
import qualified Data.Map                     as Map
import           Data.Word                    (Word8)
import           System.Environment           (getEnv)
import           System.IO                    (hGetLine, writeFile)
import           System.Process               (CreateProcess (std_in, std_out),
                                               StdStream (CreatePipe),
                                               createProcess, proc)
import           System.Random                (randomIO)
import           Text.Read                    (readMaybe)

import           TinyRAM.Bytes                (bytesPerWord)
import           TinyRAM.DecodeInstruction    (decodeInstruction)
import           TinyRAM.Disassembler         (disassembleProgram, pairWords)
import           TinyRAM.Run                  (run)
import           TinyRAM.Spec.Gen             (genMachineState)
import           TinyRAM.Spec.Prelude
import           TinyRAM.Types.Address        (Address (..))
import           TinyRAM.Types.Flag           (Flag (..))
import           TinyRAM.Types.InputTape      (Auxiliary, InputTape (..),
                                               Primary)
import           TinyRAM.Types.MaxSteps       (MaxSteps (..))
import           TinyRAM.Types.MemoryValues   (MemoryValues (..))
import           TinyRAM.Types.Params         (Params (..))
import           TinyRAM.Types.Program        (Program (..))
import           TinyRAM.Types.ProgramCounter (ProgramCounter (..))
import           TinyRAM.Types.Register       (Register (..))
import           TinyRAM.Types.RegisterCount  (RegisterCount (..))
import           TinyRAM.Types.RegisterValues (RegisterValues (..))
import           TinyRAM.Types.TinyRAMT       (TinyRAMT (..))
import           TinyRAM.Types.Word           (Word (..))
import           TinyRAM.Types.WordSize       (WordSize (..))


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
  it "passes a smoke test" $ do
    result <- runCoqTinyRAM (Program "\xFC\0\0\0") (InputTape []) (InputTape []) (MaxSteps 6)
    result `shouldBe` (Just 0)
    return ()


answerTest :: Spec
answerTest =
  it "answers 4" $ do
    result <- runCoqTinyRAM (Program "\xFC\0\0\x04") (InputTape []) (InputTape []) (MaxSteps 6)
    result `shouldBe` (Just 4)


answerRegisterTest :: Spec
answerRegisterTest =
  it "answers 0" $ do
    result <- runCoqTinyRAM (Program "\xF8\0\0\x02") (InputTape []) (InputTape []) (MaxSteps 5)
    result `shouldBe` (Just 0)


readFromPrimaryTapeTest :: Spec
readFromPrimaryTapeTest =
  it "reads from the primary input tape and provides output" $ do
    result <- runCoqTinyRAM (Program "\xF4\0\0\0\xF8\0\0\0") (InputTape [2]) (InputTape []) (MaxSteps 6)
    result `shouldBe` (Just 2)


readFromSecondaryTapeTest :: Spec
readFromSecondaryTapeTest =
  it "reads from the secondary input tape and provides output" $ do
    result <- runCoqTinyRAM (Program "\xF4\0\0\x01\xF8\0\0\0") (InputTape []) (InputTape [2]) (MaxSteps 6)
    result `shouldBe` (Just 2)


emptyReadTest :: Spec
emptyReadTest =
  it "does not crash when reading from an empty tape" $ do
    result <- runCoqTinyRAM (Program "\xF4\0\0\0\xF8\0\0\0") (InputTape []) (InputTape []) (MaxSteps 6)
    result `shouldBe` (Just 0)


invalidReadTest :: Spec
invalidReadTest =
  it "does not crash when reading from a non-existent tape with random crud in the unused part of the instruction" $ do
    result <- runCoqTinyRAM (Program "\xF4\x44\x44\x44\xF8\0\0\0") (InputTape []) (InputTape []) (MaxSteps 6)
    result `shouldBe` (Just 0)


programCounterPastEndTest :: Spec
programCounterPastEndTest =
  it "answers 1 if the program counter goes past the end of the program" $ do
    result <- runCoqTinyRAM (Program "\xD8\x00\xFF\xF8\xF8\0\0\0") (InputTape []) (InputTape []) (MaxSteps 6)
    result `shouldBe` (Just 1)


generatedTests :: Spec
generatedTests =
  it "produces the same result as the Haskell TinyRAM emulator" $
    forAll (genMachineState ws rc) $ \s' ->
      forAll (MaxSteps <$> choose (0,1000)) $ \maxSteps -> do
        let s = (#programCounter .~ ProgramCounter 0)
              . (#registerValues .~ RegisterValues
                  (Map.fromList
                    (take (unRegisterCount rc)
                      (zip (Register <$> [0..]) (Word <$> repeat 0)))))
              . (#conditionFlag .~ Flag 0)
              . (#memoryValues .~ MemoryValues
                  (Map.fromList
                    (take (2 ^ unWordSize ws)
                      (zip (Address . Word . (* fromIntegral (bytesPerWord ws)) <$> [0..])
                           (Word <$> repeat 0)))))
              $ s'
            progWords = (Map.elems (s ^. #programMemoryValues . #unProgramMemoryValues))
            prog = Program $ wordsToBytesBigEndian ws progWords
            instructions = decodeInstruction ws rc  <$> pairWords progWords
        writeFile "/tmp/run-coq-tinyram-asm" (disassembleProgram ws rc instructions)
        coqResult <- runCoqTinyRAM prog (s ^. #primaryInput) (s ^. #auxiliaryInput) maxSteps
        when False $ putStrLn (show coqResult)
        let hsResult = runIdentity . runExceptT . runStateT (unTinyRAMT (run (Just maxSteps))) $ (ps, s)
        when False $ putStrLn (show hsResult)
        case (coqResult, hsResult) of
          (_, Left _)       -> return () -- TODO more granularly compare error results
          (x, Right (y, _)) -> x `shouldBe` y
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


runCoqTinyRAM :: Program
              -> InputTape Primary
              -> InputTape Auxiliary
              -> MaxSteps
              -> IO (Maybe Word)
runCoqTinyRAM (Program p)
              (InputTape ip)
              (InputTape ia)
              (MaxSteps maxSteps) = do
  suffix :: Int <- randomIO
  let wordSize = 16
      tmpPath1 = "/tmp/run-coq-tinyram-prog-" <> show suffix
      tmpPath2 = "/tmp/run-coq-tinyram-primary-input-" <> show suffix
      tmpPath3 = "/tmp/run-coq-tinyram-secondary-input-" <> show suffix
  writeFile tmpPath1 (bytesToBitString p)
  writeFile tmpPath2 (bytesToBitString (wordsToBytesBigEndian wordSize ip))
  writeFile tmpPath3 (bytesToBitString (wordsToBytesBigEndian wordSize ia))
  (mpStdin, mpStdout, _pStderr, _pHandle) <- do
    exePath <- getEnv "COQ_TINYRAM_PATH"
    createProcess
      ((proc exePath [tmpPath1, tmpPath2, tmpPath3, show maxSteps])
        { std_in = CreatePipe, std_out = CreatePipe })
  case (mpStdin, mpStdout) of
    (_, Just pStdout) -> do
      _ <- hGetLine pStdout -- discard useless first line of output
      o1 <- hGetLine pStdout
      when False $ putStrLn o1
      o2 <- hGetLine pStdout
      when False $ putStrLn o2
      o3 <- hGetLine pStdout
      when False $ putStrLn o3
      if isPrefixOf "Error: Program did not halt within" o3
        then return Nothing
        else do
          o4 <- hGetLine pStdout
          when False $ putStrLn o4
          o5 <- hGetLine pStdout
          when False $ putStrLn o5
          if isPrefixOf "\tNat: " o5
            then return (Word <$> readMaybe (drop 6 o5))
            else return Nothing
    _ -> return Nothing


bytesToBitString :: ByteString -> String
bytesToBitString = concatMap byteToBitString . unpack


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


byteToBitString :: Word8 -> String
byteToBitString w =
  [ if testBit w i then '1' else '0'
  | i <- reverse [0..7]
  ]


wordsToBytesBigEndian :: WordSize -> [Word] -> ByteString
wordsToBytesBigEndian ws wrds =
  BS.concat $ wordsToByte <$> wrds
  where
    bytesPerWord' = bytesPerWord ws

    shiftValue = (2 :: Word) ^ (8 :: Word)

    -- encode words to little endian format
    wordsToByte :: Word -> ByteString
    wordsToByte wrd =
      fst $ foldr
              (\_ (a, cw) -> (flip BS.cons a (fromIntegral $ cw .&. (shiftValue - 1)), cw `div` shiftValue))
              (BS.empty, wrd)
              [1..bytesPerWord']
