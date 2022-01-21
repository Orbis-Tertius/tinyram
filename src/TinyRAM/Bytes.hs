{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Bytes (bytesToWords) where


import qualified Data.ByteString        as BS

import           TinyRAM.Prelude
import           TinyRAM.Types.Word     (Word)
import           TinyRAM.Types.WordSize (WordSize (..))


bytesToWords :: WordSize -> ByteString -> [Word]
bytesToWords (WordSize ws) bytes =
    bytesToWord <$> wordBytes
  where
    bytesPerWord = ws `quot` 8

    -- decode word from little endian format
    bytesToWord :: ByteString -> Word
    bytesToWord =
      BS.foldr
      (\a x -> x * (2 ^ ws) + fromIntegral a)
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
