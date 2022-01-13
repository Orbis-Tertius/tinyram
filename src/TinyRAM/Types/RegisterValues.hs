{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.RegisterValues ( RegisterValues (..) ) where


import TinyRAM.Prelude
import TinyRAM.Types.Register (Register)
import TinyRAM.Types.Word (Word)


newtype RegisterValues = RegisterValues
  { unRegisterValues :: Map Register Word }
  deriving (Eq, Ord, Read, Show, Generic)
