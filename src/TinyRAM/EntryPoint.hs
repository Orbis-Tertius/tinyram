{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}


module TinyRAM.EntryPoint ( main ) where


import qualified Data.ByteString as BS
import Data.Text (unpack)
import qualified Options.Applicative as O

import TinyRAM.Bytes (bytesToWords)
import TinyRAM.ExecuteProgram (executeProgram)
import TinyRAM.Prelude
import TinyRAM.Types.Command (Command (CommandRun))
import TinyRAM.Types.InputTape (InputTape (..), Primary, Auxiliary)
import TinyRAM.Types.InputTapePath (InputTapePath (..))
import TinyRAM.Types.MaxSteps (MaxSteps (..))
import TinyRAM.Types.Params (Params (..))
import TinyRAM.Types.Program (Program (..))
import TinyRAM.Types.ProgramFilePath (ProgramFilePath (..))
import TinyRAM.Types.RegisterCount (RegisterCount (..))
import TinyRAM.Types.Word (Word (..))
import TinyRAM.Types.WordSize (WordSize (..))



wordSize :: O.Parser WordSize
wordSize = O.option (WordSize <$> O.auto)
  (O.short 'w' <>
   O.long "word-size" <>
   O.value 16 <>
   O.help "The word / address space size in bits (default: 16)")


registerCount :: O.Parser RegisterCount
registerCount = O.option (RegisterCount <$> O.auto)
  (O.short 'r' <>
   O.long "register-count" <>
   O.value 16 <>
   O.help "The register count (default: 16)")


maxSteps :: O.Parser (Maybe MaxSteps)
maxSteps = O.option (Just . MaxSteps <$> O.auto)
  (O.short 'm' <>
   O.long "max-steps" <>
   O.value Nothing <>
   O.help "The maximum number of instructions to execute before giving up (default: no maximum)")


programFilePath :: O.Parser ProgramFilePath
programFilePath =
  O.argument (ProgramFilePath <$> O.str)
  (O.metavar "PROGRAM" <>
   O.help "The path to the program binary file")


primaryInputTapePath :: O.Parser (InputTapePath Primary)
primaryInputTapePath =
  O.argument (InputTapePath <$> O.str)
  (O.metavar "INPUT_PRIMARY" <>
   O.help "The path to the primary input tape binary file")


auxiliaryInputTapePath :: O.Parser (InputTapePath Auxiliary)
auxiliaryInputTapePath =
  O.argument (InputTapePath <$> O.str)
  (O.metavar "INPUT_AUXILIARY" <>
   O.help "The path to the auxiliary input tape binary file")


runP :: O.Parser Command
runP =
  CommandRun
  <$> (Params <$> wordSize <*> registerCount)
  <*> maxSteps
  <*> programFilePath
  <*> primaryInputTapePath
  <*> auxiliaryInputTapePath


command :: O.ParserInfo Command
command = O.info (runP O.<**> O.helper)
  (O.fullDesc <>
   O.progDesc "Run a TinyRAM program" <>
   O.header "tinyram - vnTinyRAM emulator")


readInputTapeFile :: WordSize -> InputTapePath a -> IO (InputTape a)
readInputTapeFile ws (InputTapePath path) =
  InputTape . bytesToWords ws <$> BS.readFile path


readProgramFile :: ProgramFilePath -> IO Program
readProgramFile (ProgramFilePath path) =
  Program <$> BS.readFile path


main :: IO ()
main = do
  cmd <- O.execParser command
  let ws = cmd ^. #params . #wordSize
  program <- readProgramFile (cmd ^. #programFilePath)
  primaryInput <- readInputTapeFile ws (cmd ^. #primaryInputTapePath)
  auxInput <- readInputTapeFile ws (cmd ^. #auxiliaryInputTapePath)
  case executeProgram (cmd ^. #params) (cmd ^. #maxSteps) program primaryInput auxInput of
    Left err -> putStrLn . unpack $ "Error: " <> err
    Right answer -> putStrLn $ "Answer: " <> show (unWord answer)
