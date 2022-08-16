{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.SignedArithmetic
  ( getSign,
    getUnsignedComponent,
    decodeSignedInt,
    signedMultiplyHigh,
  )
where

import TinyRAM.Prelude
import TinyRAM.Types.Sign (Sign (..))
import TinyRAM.Types.SignedInt (SignedInt (..))
import TinyRAM.Types.UnsignedInt (UnsignedInt (..))
import TinyRAM.Types.Word (Word (..))
import TinyRAM.Types.WordSize (WordSize (..))

getSign :: WordSize -> SignedInt -> Sign
getSign ws x =
  case Word (2 ^ (fromIntegral ws - 1 :: Integer)) .&. unSignedInt x of
    0 -> Sign 1
    _ -> Sign (-1)

getUnsignedComponent :: WordSize -> SignedInt -> UnsignedInt
getUnsignedComponent ws x =
  UnsignedInt . Word $ abs (decodeSignedInt ws x)

-- Decode the two's complement representation to get the value of the SignedInt.
decodeSignedInt :: WordSize -> SignedInt -> Integer
decodeSignedInt ws (SignedInt (Word x)) =
  (x .&. (2 ^ (fromIntegral ws - 1 :: Integer) - 1))
    - (x .&. (2 ^ (fromIntegral ws - 1 :: Integer)))

signedMultiplyHigh :: WordSize -> SignedInt -> SignedInt -> SignedInt
signedMultiplyHigh ws x y =
  let xSign = getSign ws x
      ySign = getSign ws y
      xAbs = getUnsignedComponent ws x
      yAbs = getUnsignedComponent ws y
      zSign =
        if x == SignedInt 0 || y == SignedInt 0
          then 1
          else xSign * ySign
      zAbs = xAbs * yAbs
      signBit = case zSign of
        -1 -> 2 ^ (ws - 1)
        _ -> 0
   in SignedInt $ signBit .|. (unUnsignedInt zAbs `shift` negate (unWordSize ws))
