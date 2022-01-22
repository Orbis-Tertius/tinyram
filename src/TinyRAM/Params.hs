{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}


module TinyRAM.Params
  ( getRegisterCount
  , getWordSize
  , getWordSizeBitmask
  , getWordSizeBitmaskMSB
  ) where


import           TinyRAM.Prelude
import           TinyRAM.Types.HasParams     (HasParams (getParams))
import           TinyRAM.Types.RegisterCount (RegisterCount)
import           TinyRAM.Types.Word          (Word)
import           TinyRAM.Types.WordSize      (WordSize (..))


getWordSize :: ( Functor m, HasParams m ) => m WordSize
getWordSize = (^. #wordSize) <$> getParams


getRegisterCount :: ( Functor m, HasParams m ) => m RegisterCount
getRegisterCount = (^. #registerCount) <$> getParams


getWordSizeBitmask :: ( Functor m, HasParams m ) => m Word
getWordSizeBitmask = f <$> getWordSize
  where
    f :: WordSize -> Word
    f (WordSize n) = (2 ^ n) - 1


getWordSizeBitmaskMSB :: ( Functor m, HasParams m ) => m Word
getWordSizeBitmaskMSB = f <$> getWordSize
  where
    f :: WordSize -> Word
    f (WordSize n) = (2 ^ n) * ((2 ^ n) - 1)
