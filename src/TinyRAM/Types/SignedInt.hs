{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}


module TinyRAM.Types.SignedInt ( SignedInt (..) ) where


import           TinyRAM.Prelude
import           TinyRAM.Types.Word (Word)


-- A word representing a signed integer.
newtype SignedInt = SignedInt { unSignedInt :: Word }
  deriving (Eq, Read, Show, Generic, Enum)
