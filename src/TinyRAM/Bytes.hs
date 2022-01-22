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

    -- decode word from little endian format
    bytesToWord :: ByteString -> TinyRAM.Types.Word.Word
    bytesToWord =
      snd . BS.foldr
            (\a (c, x) -> (c+1, x * (2 ^ (8 * c)) + fromIntegral a))
            (1, 0)

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

    -- encode words to little endian format
    wordsToByte :: Word -> ByteString
    wordsToByte wrd =
      foldr
      (\x a -> if x == 1
                  then BS.cons (fromIntegral $ wrd .&. (2 ^ 8 - 1)) a
                  else BS.cons (fromIntegral $ (wrd `div` (2 ^ (8 * x))) .&. (2 ^ 8 - 1)) a)
      BS.empty
      [1..bytesPerWord]
