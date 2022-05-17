{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module TinyRAM.Spec.CoqTinyRAMSpec
  ( byteToBitString
  , spec
  ) where


import Data.Bits (testBit)
import Data.ByteString (unpack, pack, writeFile)
import Data.Word (Word8)
import System.Process (createProcess, proc)

import TinyRAM.Types.Program (Program (..))
import TinyRAM.Types.InputTape (InputTape (..), Primary, Auxiliary)
import TinyRAM.Spec.Prelude


spec :: Spec
spec = do
  byteToBitStringSpec
  bytesToBitStringSpec
  coqTinyRAMSpec


coqTinyRAMSpec :: Spec
coqTinyRAMSpec =
  describe "coq-tinyram" $ do
    coqTinyRAMSmokeTest


coqTinyRAMSmokeTest :: Spec
coqTinyRAMSmokeTest =
  it "passes a smoke test" $ do
    runCoqTinyRAM (Program "\0\0\0\0") (InputTape []) (InputTape [])


bytesToBitStringSpec :: Spec
bytesToBitStringSpec =
  describe "bytesToBitString" $
    it "works as expected on an example" $
      bytesToBitString (pack [0, 13, 255, 64])
        `shouldBe` "00000000000011011111111101000000"


runCoqTinyRAM :: Program
              -> InputTape Primary
              -> InputTape Auxiliary
              -> IO ()
runCoqTinyRAM (Program p) (InputTape _ip) (InputTape _ia) = do
  let tmpPath = "/tmp/run-coq-tinyram"
  writeFile tmpPath p
  (_pStdin, _pStdout, _pStderr, _pHandle) <-
    createProcess (proc "/nix/store/qw141iqvfrfi403cv8y528inwl1d33kn-coq-tinyram-0.1.0.0/bin/coq-tinyram" [tmpPath])
  return ()


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
