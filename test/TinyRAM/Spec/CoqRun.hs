{-# LANGUAGE ScopedTypeVariables #-}

module TinyRAM.Spec.CoqRun (runCoqTinyRAM, byteToBitString, bytesToBitString, wordsToBytesBigEndian, toProgram) where

import Data.Bits
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Word (Word8)
import System.Environment (getEnv)
import System.IO (hGetLine)
import System.Process
  ( CreateProcess (std_in, std_out),
    StdStream (CreatePipe),
    createProcess,
    proc,
  )
import System.Random (randomIO)
import Text.Read (readMaybe)
import TinyRAM.Bytes (bytesPerWord)
import TinyRAM.EncodeInstruction
import TinyRAM.Types.InputTape
  ( Auxiliary,
    InputTape (..),
    Primary,
  )
import TinyRAM.Types.MaxSteps
import TinyRAM.Types.Program
import TinyRAM.Types.ProgramMemoryValues
import TinyRAM.Types.RegisterCount
import TinyRAM.Types.Word
import TinyRAM.Types.WordSize
import Prelude hiding (Word)

wordsToBytesBigEndian :: WordSize -> [Word] -> BS.ByteString
wordsToBytesBigEndian ws wrds =
  BS.concat $ wordsToByte <$> wrds
  where
    bytesPerWord' = bytesPerWord ws

    shiftValue = (2 :: Word) ^ (8 :: Word)

    -- encode words to little endian format
    wordsToByte :: Word -> BS.ByteString
    wordsToByte wrd =
      fst $
        foldr
          ( \_ (a, cw) ->
              ( BS.cons
                  (fromIntegral $ cw .&. (shiftValue - 1))
                  a,
                cw `div` shiftValue
              )
          )
          (BS.empty, wrd)
          [1 .. bytesPerWord']

byteToBitString :: Word8 -> String
byteToBitString w =
  [ if testBit w i then '1' else '0'
    | i <- reverse [0 .. 7]
  ]

bytesToBitString :: BS.ByteString -> String
bytesToBitString = concatMap byteToBitString . BS.unpack

toProgram :: WordSize -> RegisterCount -> ProgramMemoryValues -> Program
toProgram ws rc (ProgramMemoryValues values) =
  let encode = encodeInstruction ws rc
      progWords = (\(x, y) -> [x, y]) . encode =<< Map.elems values
      prog = Program $ wordsToBytesBigEndian ws progWords
   in prog

runCoqTinyRAM ::
  Program ->
  InputTape Primary ->
  InputTape Auxiliary ->
  MaxSteps ->
  IO (Maybe Word)
runCoqTinyRAM
  (Program p)
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
        ( (proc exePath [tmpPath1, tmpPath2, tmpPath3, show maxSteps])
            { std_in = CreatePipe,
              std_out = CreatePipe
            }
        )
    case (mpStdin, mpStdout) of
      (_, Just pStdout) -> do
        _ <- hGetLine pStdout -- discard useless first line of output
        _ <- hGetLine pStdout
        -- putStrLn o1
        _ <- hGetLine pStdout
        -- putStrLn o2
        o3 <- hGetLine pStdout
        -- putStrLn o3
        if "Error: Program did not halt within" `isPrefixOf` o3
          then return Nothing
          else do
            _ <- hGetLine pStdout
            -- putStrLn o4
            o5 <- hGetLine pStdout
            -- putStrLn o5
            if isPrefixOf "\tNat: " o5
              then return (Word <$> readMaybe (drop 6 o5))
              else return Nothing
      _ -> return Nothing
