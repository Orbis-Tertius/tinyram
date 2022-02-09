{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module TinyRAM.EntryPoint ( main ) where


import           Control.Applicative           ((<|>))
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BC
import           Data.Text                     (unpack)
import qualified Options.Applicative           as O
import           Text.Parsec                   (runParser)

import           TinyRAM.Bytes                 (bytesToWords)
import           TinyRAM.EncodeInstruction     (encodeInstruction)
import           TinyRAM.ExecuteProgram        (executeProgram)
import           TinyRAM.Parser                (firstLine, instruction)
import           TinyRAM.Prelude
import           TinyRAM.Types.Command         (Command (..))
import           TinyRAM.Types.InputTape       (Auxiliary, InputTape (..),
                                                Primary)
import           TinyRAM.Types.InputTapePath   (InputTapePath (..))
import           TinyRAM.Types.MaxSteps        (MaxSteps (..))
import           TinyRAM.Types.Params          (Params (..))
import           TinyRAM.Types.Program         (Program (..))
import           TinyRAM.Types.ProgramFilePath (ProgramFilePath (..))
import           TinyRAM.Types.RegisterCount   (RegisterCount (..))
import           TinyRAM.Types.Word            (Word (..))
import           TinyRAM.Types.WordSize        (WordSize (..))



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

programInputFilePath :: O.Parser ProgramFilePath
programInputFilePath =
  O.argument (ProgramFilePath <$> O.str)
  (O.metavar "PROGRAM" <>
   O.help "The path to the instructions program file")

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
  ( CommandParse
  <$> programInputFilePath
  <*> programFilePath
  ) <|>
  (CommandRun
  <$> (Params <$> wordSize <*> registerCount)
  <*> maxSteps
  <*> programFilePath
  <*> primaryInputTapePath
  <*> auxiliaryInputTapePath
  )


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

appendProgramFile :: ProgramFilePath -> ByteString -> IO ()
appendProgramFile (ProgramFilePath path) value =
  BS.appendFile path (value <> "\n")


main :: IO ()
main = do
  pCmd <- O.execParser command
  case pCmd of
    CommandRun params' maxSteps' pf pitp atp -> do
      let ws = params' ^. #wordSize
      program <- readProgramFile pf
      primaryInput <- readInputTapeFile ws pitp
      auxInput <- readInputTapeFile ws atp
      case executeProgram params' maxSteps' program primaryInput auxInput of
        Left err     -> putStrLn . unpack $ "Error: " <> err
        Right answer -> putStrLn $ "Answer: " <> show (unWord answer)
    CommandParse inputFile outputFile -> do
      program <- lines . BC.unpack . unProgram <$> readProgramFile inputFile
      case runParser firstLine () "First line" (head program) of
        Right (ws, rcount) ->
          mapM_
            (\x -> case runParser instruction () ("instruction: " <> x) x of
                    Right (Just ins) ->
                      appendProgramFile
                        outputFile
                        (BC.pack . toBinary (2 * unWordSize ws) . unWord $ encodeInstruction ins ws rcount)
                    Right Nothing -> putStrLn $ "Parse Failed: " <> x
                    Left err -> putStrLn $ "Error: " <> show err
            )
            (tail program)
        Left err -> putStrLn $ "Error: " <> show err
