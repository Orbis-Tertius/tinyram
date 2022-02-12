{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}


module TinyRAM.Types.Word ( Word (..) ) where


import           ConCat.Circuit
import           ConCat.Rep

import           TinyRAM.Prelude


-- A word, i.e. a sequence of WordSize many bits,
-- represented as a non-negative integer,
-- which stands for the bit sequence which
-- is the integer's binary representation.
newtype Word = Word { unWord :: Integer }
  deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral, Bits)

instance Validity Word where
  validate (Word ws) = validate ws

instance HasRep Word where
  type Rep Word = Integer
  repr (Word a) = a
  abst a =  Word a

instance GenBuses Word where
  genBuses' = genBusesRep'
  ty = tyRep @Word
  unflattenB' = genUnflattenB'
