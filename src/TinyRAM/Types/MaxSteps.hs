{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}


module TinyRAM.Types.MaxSteps ( MaxSteps (..) ) where


import           TinyRAM.Prelude


-- The maximum number of instructions which the emulator is allowed
-- to execute in running a program.
newtype MaxSteps = MaxSteps { unMaxSteps :: Integer }
  deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral)
