{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Cast
  ( word8ToWord,
    word8ToInt,
    wordToWord8,
    intToInteger,
    integerToInt,
    intToProgramCounter,
    wordToInt,
    wordSizetoInt,
    wordSizeToWord,
    wordSizeToInteger,
    intToAddress,
    addressToInt,
    word16ToInteger,
    unsignedIntToInt,
    wordSizeToUnsignedInt,
    intToChar,
  )
where

import Data.Bits (toIntegralSized)
import GHC.Word (Word16, Word8)
import Safe (toEnumMay)
import TinyRAM.Die (die)
import TinyRAM.Prelude
import TinyRAM.Types.Address (Address (..))
import TinyRAM.Types.ProgramCounter (ProgramCounter (..))
import TinyRAM.Types.UnsignedInt (UnsignedInt (..))
import TinyRAM.Types.Word (Word (..))
import TinyRAM.Types.WordSize (WordSize (..))

word8ToWord :: Word8 -> Word
word8ToWord = Word . fromMaybe (die "word8ToWord partiality") . toIntegralSized

word8ToInt :: Word8 -> Int
word8ToInt = fromMaybe (die "word8ToInt partiality") . toIntegralSized

wordToWord8 :: Word -> Word8
wordToWord8 (Word w) = fromMaybe (die "wordtoWord8 partiality") (toIntegralSized w)

intToInteger :: Int -> Integer
intToInteger = fromMaybe (die "intToInteger partiality") . toIntegralSized

intToProgramCounter :: Int -> ProgramCounter
intToProgramCounter = ProgramCounter . Address . Word . intToInteger

integerToInt :: Integer -> Int
integerToInt = fromMaybe (die "integerToInt partiality") . toIntegralSized

wordToInt :: Word -> Int
wordToInt = integerToInt . unWord

wordSizeToWord :: WordSize -> Word
wordSizeToWord = Word . intToInteger . unWordSize

wordSizeToInteger :: WordSize -> Integer
wordSizeToInteger = intToInteger . unWordSize

intToAddress :: Int -> Address
intToAddress = Address . Word . intToInteger

addressToInt :: Address -> Int
addressToInt = wordToInt . unAddress

word16ToInteger :: Word16 -> Integer
word16ToInteger = fromMaybe (die "word16ToInteger partiality") . toIntegralSized

unsignedIntToInt :: UnsignedInt -> Int
unsignedIntToInt = wordToInt . unUnsignedInt

wordSizeToUnsignedInt :: WordSize -> UnsignedInt
wordSizeToUnsignedInt = UnsignedInt . Word . wordSizeToInteger

intToChar :: Int -> Char
intToChar = fromMaybe (die "intToChar partiality") . toEnumMay

wordSizetoInt :: WordSize -> Int
wordSizetoInt = integerToInt . wordSizeToInteger
