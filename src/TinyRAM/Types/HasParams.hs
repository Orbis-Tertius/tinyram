{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}


module TinyRAM.Types.HasParams
  ( HasParams (getParams)
  , getWordSize
  , getRegisterCount
  ) where


import TinyRAM.Prelude
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.RegisterCount (RegisterCount)
import TinyRAM.Types.WordSize (WordSize)


class HasParams m where
  getParams :: m Params


getWordSize :: ( Functor m, HasParams m ) => m WordSize
getWordSize = (^. #wordSize) <$> getParams


getRegisterCount :: ( Functor m, HasParams m ) => m RegisterCount
getRegisterCount = (^. #registerCount) <$> getParams
