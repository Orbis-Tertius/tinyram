{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.DecodeInstruction
  ( decodeInstruction,
    bitsPerRegister,
  )
where

import TinyRAM.Cast (integerToInt, wordSizetoInt, wordToInt, intToInteger)
import Crypto.Number.Basic (log2)
import Data.Bits (rotate)
import TinyRAM.Prelude
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (..))
import TinyRAM.Types.Instruction (Instruction (..))
import TinyRAM.Types.Register (Register (..))
import TinyRAM.Types.RegisterCount (RegisterCount (..))
import TinyRAM.Types.Word (Word (..))
import TinyRAM.Types.WordSize (WordSize)


decodeInstruction :: WordSize -> RegisterCount -> (Word, Word) -> Maybe Instruction
decodeInstruction ws rc i@(i0, _) =
  case decodeOpcode ws i0 of
    0 -> Just $ And ri rj a
    1 -> Just $ Or ri rj a
    2 -> Just $ Xor ri rj a
    3 -> Just $ Not ri a
    4 -> Just $ Add ri rj a
    5 -> Just $ Sub ri rj a
    6 -> Just $ Mull ri rj a
    7 -> Just $ Umulh ri rj a
    8 -> Just $ Smulh ri rj a
    9 -> Just $ Udiv ri rj a
    10 -> Just $ Umod ri rj a
    11 -> Just $ Shl ri rj a
    12 -> Just $ Shr ri rj a
    13 -> Just $ Cmpe ri a
    14 -> Just $ Cmpa ri a
    15 -> Just $ Cmpae ri a
    16 -> Just $ Cmpg ri a
    17 -> Just $ Cmpge ri a
    18 -> Just $ Mov ri a
    19 -> Just $ Cmov ri a
    20 -> Just $ Jmp a
    21 -> Just $ Cjmp a
    22 -> Just $ Cnjmp a
    26 -> Just $ Storeb a ri
    27 -> Just $ Loadb ri a
    28 -> Just $ Storew a ri
    29 -> Just $ Loadw ri a
    30 -> Just $ Out a
    31 -> Just $ Answer a
    _ -> Nothing
  where
    ri = decodeRI ws rc i0
    rj = decodeRJ ws rc i0
    a = decodeA ws i

decodeOpcode :: WordSize -> Word -> Int
decodeOpcode ws i1 = integerToInt opcode
  where
    opcode :: Integer
    opcode =
      (unWord i1 `rotate` (-wordSizetoInt ws + 5))
        .&. opcodeBitmask

    opcodeBitmask :: Integer
    opcodeBitmask = 31

decodeRI :: WordSize -> RegisterCount -> Word -> Register
decodeRI ws rc (Word w) =
  Register . integerToInt $
    shifted .&. registerBitmask rc
  where
    shifted :: Integer
    shifted = w `rotate` (-wordSizetoInt ws + bitsPerRegister rc + 6)

decodeRJ :: WordSize -> RegisterCount -> Word -> Register
decodeRJ ws rc (Word w) =
  Register . integerToInt $
    shifted .&. registerBitmask rc
  where
    shifted :: Integer
    shifted = w `rotate` (-wordSizetoInt ws + 2 * bitsPerRegister rc + 6)

bitsPerRegister :: RegisterCount -> Int
bitsPerRegister (RegisterCount rc) = log2 (intToInteger rc)

registerBitmask :: RegisterCount -> Integer
registerBitmask rc = 2 ^ bitsPerRegister rc - 1

decodeA :: WordSize -> (Word, Word) -> ImmediateOrRegister
decodeA ws (i0, i1) =
  if flagBitmask then IsImmediate i1 else IsRegister . Register . wordToInt $ i1
  where
    flagBitmask :: Bool
    flagBitmask = shifted .&. 1 == 1

    shifted :: Integer
    shifted = unWord i0 `rotate` (-wordSizetoInt ws + 6)
