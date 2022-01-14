{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.SignedArithmetic
  ( getSign
  , getUnsignedComponent
  ) where


import TinyRAM.Prelude
import TinyRAM.Types.Sign (Sign (..))
import TinyRAM.Types.SignedInt (SignedInt (..))
import TinyRAM.Types.Word (Word (..))
import TinyRAM.Types.WordSize (WordSize (..))
import TinyRAM.Types.UnsignedInt (UnsignedInt (..))


getSign :: WordSize -> SignedInt -> Sign
getSign ws x =
  case Word (2 ^ (fromIntegral ws - 1 :: Integer)) .&. unSignedInt x of
    0 -> Sign 1
    _ -> Sign (-1)

getUnsignedComponent :: WordSize -> SignedInt -> UnsignedInt
getUnsignedComponent ws x =
  UnsignedInt $ Word (2 ^ (fromIntegral ws - 1 :: Integer) - 1) .&. unSignedInt x
