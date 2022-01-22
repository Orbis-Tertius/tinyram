{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Bytes (bytesToWords, wordsToBytes) where


import qualified Data.ByteString as BS

import TinyRAM.Prelude
import TinyRAM.Types.Word (Word)
import TinyRAM.Types.WordSize (WordSize (..))

bytesToWords :: WordSize -> ByteString -> [Word]
bytesToWords (WordSize ws) bytes =
    bytesToWord <$> wordBytes
  where
    bytesPerWord = ws `quot` 8

    shiftValue = (2 :: Word) ^ (8 :: Word)

    -- decode word from little endian format
    bytesToWord :: ByteString -> Word
    bytesToWord =
      BS.foldr
      (\a x -> x * shiftValue + fromIntegral a)
      0

    wordBytes :: [ByteString]
    wordBytes = f bytes

    f :: ByteString -> [ByteString]
    f bs =
      if BS.length bs < bytesPerWord
      then []
      else
        let (wb,bs') = BS.splitAt bytesPerWord bs
        in wb : f bs'

wordsToBytes :: WordSize -> [Word] -> ByteString
wordsToBytes (WordSize ws) wrds =
  BS.concat $ wordsToByte <$> wrds
  where
    bytesPerWord = ws `quot` 8

    shiftValue = (2 :: Word) ^ (8 :: Word)

    -- encode words to little endian format
    wordsToByte :: Word -> ByteString
    wordsToByte wrd =
      fst $ foldr
              (\_ (a, cw) -> (BS.snoc a (fromIntegral $ cw .&. (shiftValue - 1)), cw `div` shiftValue))
              (BS.empty, wrd)
              [1..bytesPerWord]
