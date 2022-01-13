{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}


module TinyRAM.Params
  ( getRegisterCount
  , getWordSize
  ) where


import TinyRAM.Prelude
import TinyRAM.Types.HasParams (HasParams (getParams))
import TinyRAM.Types.RegisterCount (RegisterCount)
import TinyRAM.Types.WordSize (WordSize)


getWordSize :: ( Functor m, HasParams m ) => m WordSize
getWordSize = (^. #wordSize) <$> getParams


getRegisterCount :: ( Functor m, HasParams m ) => m RegisterCount
getRegisterCount = (^. #registerCount) <$> getParams
