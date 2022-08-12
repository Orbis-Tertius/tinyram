{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.EntryPoint
  ( main,
    handleCommand,
    readAssemblyFile,
    readObjectFile,
  )
where

import qualified Data.ByteString as BS
import Data.Text (unpack)
import Numeric (showHex)
import qualified Options.Applicative as O
import TinyRAM.Bytes (bytesToWords)
import TinyRAM.ExecuteProgram (executeProgram)
import TinyRAM.Prelude
import TinyRAM.Types.Command (Command (..))
import TinyRAM.Types.InputTape
  ( Auxiliary,
    InputTape (..),
    Primary,
  )
import TinyRAM.Types.InputTapePath (InputTapePath (..))
import TinyRAM.Types.MaxSteps (MaxSteps (..))
import TinyRAM.Types.Params (Params (..))
import TinyRAM.Types.Program (Program (..))
import TinyRAM.Types.ProgramFilePath
  ( AssemblyFilePath (..),
    ObjectFilePath (..),
  )
import TinyRAM.Types.RegisterCount (RegisterCount (..))
import TinyRAM.Types.Word (Word (..))
import TinyRAM.Types.WordSize (WordSize (..))

wordSize :: O.Parser WordSize
wordSize =
  O.option
    (WordSize <$> O.auto)
    ( O.short 'w'
        <> O.long "word-size"
        <> O.value 16
        <> O.help "The word / address space size in bits (default: 16)"
    )

registerCount :: O.Parser RegisterCount
registerCount =
  O.option
    (RegisterCount <$> O.auto)
    ( O.short 'r'
        <> O.long "register-count"
        <> O.value 16
        <> O.help "The register count (default: 16)"
    )

maxSteps :: O.Parser (Maybe MaxSteps)
maxSteps =
  O.option
    (Just . MaxSteps <$> O.auto)
    ( O.short 'm'
        <> O.long "max-steps"
        <> O.value Nothing
        <> O.help "The maximum number of instructions to execute before giving up (default: no maximum)"
    )

programFilePath :: O.Parser ObjectFilePath
programFilePath =
  O.argument
    (ObjectFilePath <$> O.str)
    ( O.metavar "PROGRAM"
        <> O.help "The path to the program binary file"
    )

primaryInputTapePath :: O.Parser (InputTapePath Primary)
primaryInputTapePath =
  O.argument
    (InputTapePath <$> O.str)
    ( O.metavar "INPUT_PRIMARY"
        <> O.help "The path to the primary input tape binary file"
    )

auxiliaryInputTapePath :: O.Parser (InputTapePath Auxiliary)
auxiliaryInputTapePath =
  O.argument
    (InputTapePath <$> O.str)
    ( O.metavar "INPUT_AUXILIARY"
        <> O.help "The path to the auxiliary input tape binary file"
    )

runP :: O.Parser Command
runP =
  CommandRun
    <$> (Params <$> wordSize <*> registerCount)
    <*> maxSteps
    <*> programFilePath
    <*> primaryInputTapePath
    <*> auxiliaryInputTapePath

command :: O.ParserInfo Command
command =
  O.info
    (runP O.<**> O.helper)
    ( O.fullDesc
        <> O.progDesc "Assemble and run TinyRAM programs"
        <> O.header "tinyram - vnTinyRAM emulator / assembler"
    )

readInputTapeFile :: WordSize -> InputTapePath a -> IO (InputTape a)
readInputTapeFile ws (InputTapePath path) =
  InputTape . bytesToWords ws <$> BS.readFile path

readAssemblyFile :: AssemblyFilePath -> IO Program
readAssemblyFile (AssemblyFilePath path) =
  Program <$> BS.readFile path

readObjectFile :: ObjectFilePath -> IO Program
readObjectFile (ObjectFilePath path) =
  Program <$> BS.readFile path

main :: IO ()
main = do
  pCmd <- O.execParser command
  handleCommand pCmd

handleCommand :: Command -> IO ()
handleCommand pCmd =
  case pCmd of
    CommandRun params' maxSteps' pf pitp atp -> do
      let ws = params' ^. #wordSize
      program <- readObjectFile pf
      primaryInput <- readInputTapeFile ws pitp
      auxInput <- readInputTapeFile ws atp
      case executeProgram params' maxSteps' program primaryInput auxInput of
        Left err -> putStrLn . unpack $ "Error: " <> err
        Right answer -> putStrLn $ "Answer: 0x" <> showHex (unWord answer) ""
