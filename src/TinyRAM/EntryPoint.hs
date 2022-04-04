{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.EntryPoint
  ( main
  , handleCommand
  , readAssemblyFile
  , readObjectFile
  , assemble
  ) where


import           Control.Applicative           ((<|>))
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BC
import           Data.Text                     (pack, unpack)
import qualified Options.Applicative           as O
import           Text.Parsec                   (runParser)

import           Numeric                       (showHex)
import           TinyRAM.Bytes                 (bytesToWords, wordsToBytes)
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
import           TinyRAM.Types.ProgramFilePath (AssemblyFilePath (..),
                                                ObjectFilePath (..))
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


programFilePath :: O.Parser ObjectFilePath
programFilePath =
  O.argument (ObjectFilePath <$> O.str)
  (O.metavar "PROGRAM" <>
   O.help "The path to the program binary file")

programInputFilePath :: O.Parser AssemblyFilePath
programInputFilePath =
  O.argument (AssemblyFilePath <$> O.str)
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
   O.progDesc "Assemble and run TinyRAM programs" <>
   O.header "tinyram - vnTinyRAM emulator / assembler")


readInputTapeFile :: WordSize -> InputTapePath a -> IO (InputTape a)
readInputTapeFile ws (InputTapePath path) =
  InputTape . bytesToWords ws <$> BS.readFile path


readAssemblyFile :: AssemblyFilePath -> IO Program
readAssemblyFile (AssemblyFilePath path) =
  Program <$> BS.readFile path

readObjectFile :: ObjectFilePath -> IO Program
readObjectFile (ObjectFilePath path) =
  Program <$> BS.readFile path

appendProgramFile :: ObjectFilePath -> ByteString -> IO ()
appendProgramFile (ObjectFilePath path) value =
  BS.appendFile path value

writeProgramFile :: ObjectFilePath -> ByteString -> IO ()
writeProgramFile (ObjectFilePath path) value =
  BS.writeFile path value


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
        Left err     -> putStrLn . unpack $ "Error: " <> err
        Right answer -> putStrLn $ "Answer: 0x" <> showHex (unWord answer) ""
    CommandParse inputFile outputFile -> do
      result <- assemble inputFile outputFile
      case result of
        Left err -> putStrLn $ "Error: " <> show err
        Right () -> return ()

-- This could look better refactored to use ExceptT.
assemble :: AssemblyFilePath -> ObjectFilePath -> IO (Either Text ())
assemble inputFile outputFile = do
  program <- lines . BC.unpack . unProgram <$> readAssemblyFile inputFile
  case runParser firstLine () "First line" (head program) of
    Right (ws, rcount) ->
      if (6 + 2 * ceiling (logBase 2 (fromIntegral rcount :: Double)) > ws)
        then return (Left "The constraint 6 + 2*ceil(log_2(K)) <= W is not satisfied.")
        else do
          writeProgramFile outputFile ""
          results :: [Either Text ()] <-
            mapM
            (\x -> case runParser instruction () ("instruction: " <> x) x of
                    Right (Just ins) -> do
                      appendProgramFile
                        outputFile
                        (wordsToBytes ws . (\(w0,w1) -> [w0,w1]) $ encodeInstruction ins ws rcount)
                      return (Right ())
                    Right Nothing -> return (Left ("Parse Failed: " <> pack x))
                    Left err -> return (Left (pack (show err)))
            )
            (tail program)
          return (sequence_ results)
    Left err -> return (Left (pack (show err)))
