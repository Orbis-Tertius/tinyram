{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Bytes (bytesToWords, wordsToBytes, bytesPerWord) where

import qualified Data.ByteString as BS
import TinyRAM.Prelude
import TinyRAM.Types.Word (Word)
import TinyRAM.Types.WordSize (WordSize (..))

bytesPerWord :: WordSize -> Int
bytesPerWord (WordSize ws) = ws `quot` 8

bytesToWords :: WordSize -> ByteString -> [Word]
bytesToWords ws bytes =
  bytesToWord <$> wordBytes
  where
    bytesPerWord' = bytesPerWord ws

    shiftValue = (2 :: Word) ^ (8 :: Word)

    -- decode word from little endian format
    bytesToWord :: ByteString -> Word
    bytesToWord =
      BS.foldr
        (\a x -> x * shiftValue + fromIntegral a)
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
          let (wb, bs') = BS.splitAt bytesPerWord' bs
           in wb : f bs'

wordsToBytes :: WordSize -> [Word] -> ByteString
wordsToBytes ws wrds =
  BS.concat $ wordsToByte <$> wrds
  where
    bytesPerWord' = bytesPerWord ws

    shiftValue = (2 :: Word) ^ (8 :: Word)

    -- encode words to little endian format
    wordsToByte :: Word -> ByteString
    wordsToByte wrd =
      fst $
        foldr
          (\_ (a, cw) -> (BS.snoc a (fromIntegral $ cw .&. (shiftValue - 1)), cw `div` shiftValue))
          (BS.empty, wrd)
          [1 .. bytesPerWord']
