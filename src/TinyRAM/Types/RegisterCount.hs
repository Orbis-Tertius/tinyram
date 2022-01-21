{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}


module TinyRAM.Types.RegisterCount ( RegisterCount (..) ) where


import           TinyRAM.Prelude


-- The total number of registers, a positive integer.
newtype RegisterCount = RegisterCount { unRegisterCount :: Int }
  deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral)
