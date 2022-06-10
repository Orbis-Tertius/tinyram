{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module TinyRAM.Spec.CoqTinyRAMSpec
  ( bytesToBitString
  , byteToBitString
  , spec
  ) where


import Data.Bits (testBit)
import Data.ByteString (pack, unpack)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import Data.Word (Word8)
import System.Environment (getEnv)
import System.IO (hGetLine, writeFile)
import System.Process (createProcess, proc, CreateProcess (std_in, std_out), StdStream (CreatePipe))
import Text.Read (readMaybe)

import TinyRAM.Bytes (bytesPerWord)
import TinyRAM.Spec.Prelude
import TinyRAM.Types.MaxSteps (MaxSteps (..))
import TinyRAM.Types.Program (Program (..))
import TinyRAM.Types.InputTape (InputTape (..), Primary, Auxiliary)
import TinyRAM.Types.Word (Word)
import TinyRAM.Types.WordSize (WordSize)


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
    readFromPrimaryTapeTest


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


readFromPrimaryTapeTest :: Spec
readFromPrimaryTapeTest =
  it "reads from the primary input tape and provides output" $ do
    result <- runCoqTinyRAM (Program "\xF4\0\0\0\xF8\0\0\0") (InputTape [2]) (InputTape []) (MaxSteps 6)
    result `shouldBe` (Just 2)


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
              -> IO (Maybe Int)
runCoqTinyRAM (Program p)
              (InputTape ip)
              (InputTape ia)
              (MaxSteps maxSteps) = do
  let wordSize = 16
      tmpPath1 = "/tmp/run-coq-tinyram-prog"
      tmpPath2 = "/tmp/run-coq-tinyram-primary-input"
      tmpPath3 = "/tmp/run-coq-tinyram-secondary-input"
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
      putStrLn o1
      o2 <- hGetLine pStdout
      putStrLn o2
      o3 <- hGetLine pStdout
      putStrLn o3
      if isPrefixOf "Error: Program did not halt within" o3
        then return Nothing
        else do
          o4 <- hGetLine pStdout
          putStrLn o4
          o5 <- hGetLine pStdout
          putStrLn o5
          if isPrefixOf "\tNat: " o5
            then return (readMaybe (drop 6 o5))
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
