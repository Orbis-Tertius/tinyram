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
