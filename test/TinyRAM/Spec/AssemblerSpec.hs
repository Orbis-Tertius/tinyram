{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.AssemblerSpec ( spec ) where


import qualified Data.ByteString.Char8         as BC
import           Test.QuickCheck.Gen           (resize)

import           TinyRAM.EntryPoint            (handleCommand, readProgramFile)
import           TinyRAM.ExecuteProgram        (executeProgram)
import           TinyRAM.Spec.Gen              (genWord)
import           TinyRAM.Spec.Prelude
import           TinyRAM.Types.Command         (Command (..))
import           TinyRAM.Types.InputTape       (InputTape (..))
import           TinyRAM.Types.Params          (Params (..))
import           TinyRAM.Types.Program         (Program (..))
import           TinyRAM.Types.ProgramFilePath (ProgramFilePath (..))
import           TinyRAM.Types.Word            (Word)


spec :: Spec
spec = describe "assembler/run" $
  before (handleCommand (CommandParse (ProgramFilePath "examples/max.s") objectFilePath)) $
    it "assembler/run max program and test against inputs" $ do
      forAll (genWord 16) $ \(a :: Word) ->
        forAll (listOf $ resize (fromIntegral a) $ genWord 15) $ \(wrds :: [Word]) -> do
            let params = Params 16 16 -- hardcoded assuming won't be changed in examples/max.s
            program <- Program . BC.concat . BC.lines . unProgram <$> readProgramFile objectFilePath
            let answer = executeProgram params (Just 1000) program (InputTape wrds) (InputTape [])
            answer `shouldBe` (Right $ maximum' $ wrds)
  where
    objectFilePath = ProgramFilePath "examples/max.o"

maximum' :: [Word] -> Word
maximum' [] = 0
maximum' xs = maximum xs
