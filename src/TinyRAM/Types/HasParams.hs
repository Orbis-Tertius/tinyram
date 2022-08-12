{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.HasParams
  ( HasParams (getParams),
  )
where

import TinyRAM.Types.Params (Params)

class HasParams m where
  getParams :: m Params
