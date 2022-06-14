{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}


module TinyRAM.Disassembler
  ( disassembleInstruction
  , disassembleProgram
  , disassembleCoqTinyRAMProgram
  , pairWords
  , bytesToWordsBigEndian
  ) where


import qualified Data.ByteString as BS
import Data.List (intercalate)
import Data.Word (Word8)

import TinyRAM.Bytes (bytesPerWord, bytesToWords)
import TinyRAM.DecodeInstruction (decodeInstruction)
import TinyRAM.Operations (getOperation)
import TinyRAM.Prelude
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (..))
import TinyRAM.Types.Instruction (Instruction (..))
import TinyRAM.Types.Opcode (Opcode (..))
import TinyRAM.Types.Register (Register (..))
import TinyRAM.Types.RegisterCount (RegisterCount (..))
import TinyRAM.Types.Word (Word (..))
import TinyRAM.Types.WordSize (WordSize (..))


disassembleInstruction :: Instruction -> String
disassembleInstruction i =
  maybe (show (unOpcode (i ^. #opcode))) show (getOperation (i ^. #opcode))
  <> " r" <> show (i ^. #ri . #unRegister)
  <> " r" <> show (i ^. #rj . #unRegister)
  <> " " <> showImmediateOrRegister (i ^. #a)


showImmediateOrRegister :: ImmediateOrRegister -> String
showImmediateOrRegister (IsImmediate i) = show (unWord i)
showImmediateOrRegister (IsRegister (Register r)) = "r" <> show r


disassembleProgram :: WordSize -> RegisterCount -> [Instruction] -> String
disassembleProgram (WordSize ws) (RegisterCount rc) is =
  "TinyRAM V=1.0000 W=" <> show ws <> " K=" <> show rc <> "\n"
  <> intercalate "\n" (disassembleInstruction <$> is)


disassembleCoqTinyRAMProgram :: String -> String
disassembleCoqTinyRAMProgram =
  disassembleProgram ws rc . fmap (decodeInstruction ws rc)
    . pairWords . bytesToWords ws . bitStringToBytes
  where
    ws = WordSize 16
    rc = RegisterCount 4


pairWords :: [Word] -> [(Word, Word)]
pairWords [] = []
pairWords [x] = [(x,0)]
pairWords (x:y:zs) = (x,y) : pairWords zs


bitStringToBytes :: String -> ByteString
bitStringToBytes [] = mempty
bitStringToBytes x =
  let (b, bs) = splitAt 8 x
  in BS.cons (bitStringToByte b) (bitStringToBytes bs)


bitStringToByte :: String -> Word8
bitStringToByte "" = 0
bitStringToByte ('1':xs) = (2 ^ length xs) + bitStringToByte xs
bitStringToByte ('0':xs) = bitStringToByte xs
bitStringToByte _ = 0


bytesToWordsBigEndian :: WordSize -> ByteString -> [Word]
bytesToWordsBigEndian (WordSize ws) bytes =
    bytesToWord <$> wordBytes
  where
    bytesPerWord' = bytesPerWord (WordSize ws)

    shiftValue = (2 :: Word) ^ (8 :: Word)

    -- decode word from big endian format
    bytesToWord :: ByteString -> Word
    bytesToWord =
      BS.foldl
      (\a x -> a * shiftValue + fromIntegral x)
      0

    misalignment :: Int
    misalignment = BS.length bytes `rem` bytesPerWord'

    alignSequence :: ByteString
    alignSequence =
      if misalignment == 0
        then BS.empty
        else BS.replicate (bytesPerWord' - misalignment) 0

    wordBytes :: [ByteString]
    wordBytes = f (bytes `BS.append` alignSequence)

    f :: ByteString -> [ByteString]
    f bs =
      if BS.length bs < bytesPerWord'
      then []
      else
        let (wb,bs') = BS.splitAt bytesPerWord' bs
        in wb : f bs'

