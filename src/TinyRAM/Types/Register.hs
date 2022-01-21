{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}


module TinyRAM.Types.Register ( Register (..) ) where


import           TinyRAM.Prelude


-- A register, represented by its zero-based index.
newtype Register = Register { unRegister :: Int }
  deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral)
