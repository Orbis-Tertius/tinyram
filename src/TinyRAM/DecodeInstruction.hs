{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.DecodeInstruction
  ( decodeInstruction
  , bitsPerRegister
  ) where


import           Data.Bits                         (rotate)

import           TinyRAM.Prelude
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (..))
import           TinyRAM.Types.Instruction         (BinOp (BinOp), Comp (Comp),
                                                    Instruction (..),
                                                    UnOp (UnOp))
import           TinyRAM.Types.Register            (Register (..))
import           TinyRAM.Types.RegisterCount       (RegisterCount (..))
import           TinyRAM.Types.Word                (Word (..))
import           TinyRAM.Types.WordSize            (WordSize)


decodeInstruction :: WordSize -> RegisterCount -> (Word, Word) -> Maybe Instruction
decodeInstruction ws rc i@(i0, _) =
  case decodeOpcode ws i0 of
    0  -> Just $ And $ BinOp ri rj a
    1  -> Just $ Or $ BinOp ri rj a
    2  -> Just $ Xor $ BinOp ri rj a
    3  -> Just $ Not $ UnOp ri a
    4  -> Just $ Add $ BinOp ri rj a
    5  -> Just $ Sub $ BinOp ri rj a
    6  -> Just $ Mull $ BinOp ri rj a
    7  -> Just $ Umulh $ BinOp ri rj a
    8  -> Just $ Smulh $ BinOp ri rj a
    9  -> Just $ Udiv $ BinOp ri rj a
    10 -> Just $ Umod$ BinOp  ri rj a
    11 -> Just $ Shl $ BinOp ri rj a
    12 -> Just $ Shr $ BinOp ri rj a

    13 -> Just $ Cmpe $ Comp ri a
    14 -> Just $ Cmpa $ Comp  ri a
    15 -> Just $ Cmpae $ Comp ri a
    16 -> Just $ Cmpg $ Comp ri a
    17 -> Just $ Cmpge$ Comp  ri a

    18 -> Just $ Mov $ UnOp ri a
    19 -> Just $ Cmov $ UnOp ri a

    20 -> Just $ Jmp a
    21 -> Just $ Cjmp a
    22 -> Just $ Cnjmp a

    26 -> Just $ Storeb a ri
    27 -> Just $ Loadb ri a
    28 -> Just $ Storew a ri
    29 -> Just $ Loadw ri a
    30 -> Just $ Read ri a
    31 -> Just $ Answer a

    _  -> Nothing

  where
    ri = decodeRI ws rc i0
    rj = decodeRJ ws rc i0
    a = decodeA ws i

decodeOpcode :: WordSize -> Word -> Int
decodeOpcode ws i1 = fromIntegral opcode
  where
    opcode :: Integer
    opcode = (fromIntegral i1 `rotate` (- fromIntegral ws + 5))
             .&. opcodeBitmask

    opcodeBitmask :: Integer
    opcodeBitmask = 31


decodeRI :: WordSize -> RegisterCount -> Word -> Register
decodeRI ws rc (Word w) = Register . fromIntegral
  $ shifted .&. registerBitmask rc
  where
    shifted :: Integer
    shifted = w `rotate` (- fromIntegral ws + bitsPerRegister rc + 6)


decodeRJ :: WordSize -> RegisterCount -> Word -> Register
decodeRJ ws rc (Word w) = Register . fromIntegral
  $ shifted .&. registerBitmask rc
  where
    shifted :: Integer
    shifted = w `rotate` (- fromIntegral ws + 2 * bitsPerRegister rc + 6)


bitsPerRegister :: RegisterCount -> Int
bitsPerRegister (RegisterCount rc) = ceiling (logBase 2 (fromIntegral rc) :: Double)


registerBitmask :: RegisterCount -> Integer
registerBitmask rc = 2 ^ bitsPerRegister rc - 1


decodeA :: WordSize -> (Word, Word) -> ImmediateOrRegister
decodeA ws (i0, i1) =
  if flagBitmask then IsImmediate i1 else IsRegister . Register . fromIntegral $ i1
  where
    flagBitmask :: Bool
    flagBitmask = shifted .&. 1 == 1

    shifted :: Integer
    shifted = fromIntegral i0 `rotate` (- fromIntegral ws + 6)
