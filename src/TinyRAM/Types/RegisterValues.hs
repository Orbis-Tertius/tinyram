{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}


module TinyRAM.Types.RegisterValues ( RegisterValues (..) ) where


import           TinyRAM.Prelude
import           TinyRAM.Types.Register (Register)
import           TinyRAM.Types.Word     (Word)


-- A state of the registers, giving the word
-- stored in each one.
newtype RegisterValues = RegisterValues
  { unRegisterValues :: Map Register Word }
  deriving (Eq, Ord, Read, Show, Generic)
