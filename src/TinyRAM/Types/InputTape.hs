{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}


module TinyRAM.Types.InputTape
  ( InputTape (..)
  , Auxiliary
  , Primary
  ) where


import           TinyRAM.Prelude
import           TinyRAM.Types.Word (Word)


data Auxiliary
data Primary


-- Represents the state of the input tape, consisting
-- of the sequence of unconsumed words on the tape.
-- Phantom type tells us if this is a primary
-- or auxiliary input tape state.
newtype InputTape a = InputTape { unInputTape :: [Word] }
  deriving (Eq, Ord, Read, Show, Generic)
