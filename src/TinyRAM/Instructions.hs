{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Instructions
  ( andBits,
    orBits,
    xorBits,
    notBits,
    addUnsigned,
    subtractUnsigned,
    multiplyUnsignedLSB,
    multiplyUnsignedMSB,
    multiplySignedMSB,
    divideUnsigned,
    modulusUnsigned,
    shiftLeft,
    shiftRight,
    compareEqual,
    compareGreaterUnsigned,
    compareGreaterOrEqualUnsigned,
    compareGreaterSigned,
    compareGreaterOrEqualSigned,
    move,
    conditionalMove,
    jump,
    jumpIfFlag,
    jumpIfNotFlag,
    store,
    storeb,
    load,
    loadb,
    readInputTape,
  )
where

import TinyRAM.Bytes (bytesPerWord)
import TinyRAM.MachineState
  ( conditionToFlag,
    getImmediateOrRegister,
    incrementProgramCounter,
  )
import TinyRAM.Params
  ( getWordSize,
    getWordSizeBitmask,
    getWordSizeBitmaskMSB,
  )
import TinyRAM.Prelude
import TinyRAM.SignedArithmetic
  ( decodeSignedInt,
    getUnsignedComponent,
    signedMultiplyHigh,
  )
import TinyRAM.Types.Address (Address (..))
import TinyRAM.Types.Flag (Flag (..))
import TinyRAM.Types.HasMachineState (HasMachineState (..))
import TinyRAM.Types.HasParams (HasParams)
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister)
import TinyRAM.Types.ProgramCounter (ProgramCounter (..))
import TinyRAM.Types.Register (Register)
import TinyRAM.Types.SignedInt (SignedInt (..))
import TinyRAM.Types.UnsignedInt (UnsignedInt (..))
import TinyRAM.Types.Word (Word (..))
import TinyRAM.Types.WordSize (WordSize (..))

andBits ::
  (HasMachineState m, HasParams m) =>
  Register ->
  Register ->
  ImmediateOrRegister ->
  m ()
andBits ri rj a = do
  a' <- getImmediateOrRegister a
  rj' <- getRegisterValue rj
  let y = a' .&. rj'
  setRegisterValue ri y
  setConditionFlag (conditionToFlag (y == 0))
  incrementProgramCounter

orBits ::
  (HasMachineState m, HasParams m) =>
  Register ->
  Register ->
  ImmediateOrRegister ->
  m ()
orBits ri rj a = do
  a' <- getImmediateOrRegister a
  rj' <- getRegisterValue rj
  let y = a' .|. rj'
  setRegisterValue ri y
  setConditionFlag (conditionToFlag (y == 0))
  incrementProgramCounter

xorBits ::
  (HasMachineState m, HasParams m) =>
  Register ->
  Register ->
  ImmediateOrRegister ->
  m ()
xorBits ri rj a = do
  a' <- getImmediateOrRegister a
  rj' <- getRegisterValue rj
  let y = a' `xor` rj'
  setRegisterValue ri y
  setConditionFlag (conditionToFlag (y == 0))
  incrementProgramCounter

notBits ::
  (HasMachineState m, HasParams m) =>
  Register ->
  ImmediateOrRegister ->
  m ()
notBits ri a = do
  WordSize w <- getWordSize
  a' <- getImmediateOrRegister a
  let y = (2 ^ w - 1) `xor` a'
  setRegisterValue ri y
  setConditionFlag (conditionToFlag (y == 0))
  incrementProgramCounter

addUnsigned ::
  (HasMachineState m, HasParams m) =>
  Register ->
  Register ->
  ImmediateOrRegister ->
  m ()
addUnsigned ri rj a = do
  a' <- UnsignedInt <$> getImmediateOrRegister a
  rj' <- UnsignedInt <$> getRegisterValue rj
  wsb <- getWordSizeBitmask
  msb <- getWordSizeBitmaskMSB
  let y = a' + rj'
  setRegisterValue ri (unUnsignedInt y .&. wsb)
  setConditionFlag (conditionToFlag (unUnsignedInt y .&. msb /= 0))
  incrementProgramCounter

subtractUnsigned ::
  (HasMachineState m, HasParams m) =>
  Register ->
  Register ->
  ImmediateOrRegister ->
  m ()
subtractUnsigned ri rj a = do
  a' <- UnsignedInt <$> getImmediateOrRegister a
  rj' <- UnsignedInt <$> getRegisterValue rj
  ws <- getWordSize
  wsb <- getWordSizeBitmask
  msb <- getWordSizeBitmaskMSB
  let k :: UnsignedInt
      k = 2 ^ (fromIntegral ws :: UnsignedInt)
      y = rj' + k - a'
  setRegisterValue ri (unUnsignedInt y .&. wsb)
  setConditionFlag (conditionToFlag (unUnsignedInt y .&. msb == 0))
  incrementProgramCounter

multiplyUnsignedLSB ::
  (HasMachineState m, HasParams m) =>
  Register ->
  Register ->
  ImmediateOrRegister ->
  m ()
multiplyUnsignedLSB ri rj a = do
  a' <- UnsignedInt <$> getImmediateOrRegister a
  rj' <- UnsignedInt <$> getRegisterValue rj
  wsb <- getWordSizeBitmask
  msb <- getWordSizeBitmaskMSB
  let y = rj' * a'
  setRegisterValue ri (unUnsignedInt y .&. wsb)
  setConditionFlag (conditionToFlag (unUnsignedInt y .&. msb /= 0))
  incrementProgramCounter

multiplyUnsignedMSB ::
  (HasMachineState m, HasParams m) =>
  Register ->
  Register ->
  ImmediateOrRegister ->
  m ()
multiplyUnsignedMSB ri rj a = do
  a' <- UnsignedInt <$> getImmediateOrRegister a
  rj' <- UnsignedInt <$> getRegisterValue rj
  ws <- getWordSize
  msb <- getWordSizeBitmaskMSB
  let y = rj' * a'
  setRegisterValue ri (shift (unUnsignedInt y) (negate (unWordSize ws)))
  setConditionFlag (conditionToFlag (unUnsignedInt y .&. msb /= 0))
  incrementProgramCounter

multiplySignedMSB ::
  (HasMachineState m, HasParams m) =>
  Register ->
  Register ->
  ImmediateOrRegister ->
  m ()
multiplySignedMSB ri rj a = do
  a' <- SignedInt <$> getImmediateOrRegister a
  rj' <- SignedInt <$> getRegisterValue rj
  ws <- getWordSize
  msb <- getWordSizeBitmaskMSB
  let aAbs = getUnsignedComponent ws a'
      rjAbs = getUnsignedComponent ws rj'
      yAbs = aAbs * rjAbs
  setRegisterValue ri . unSignedInt $
    signedMultiplyHigh ws a' rj'
  setConditionFlag (conditionToFlag (unUnsignedInt yAbs .&. msb /= 0))
  incrementProgramCounter

divideUnsigned ::
  (HasMachineState m, HasParams m) =>
  Register ->
  Register ->
  ImmediateOrRegister ->
  m ()
divideUnsigned ri rj a = do
  a' <- UnsignedInt <$> getImmediateOrRegister a
  rj' <- UnsignedInt <$> getRegisterValue rj
  let y = if a' == 0 then 0 else rj' `div` a'
  setRegisterValue ri (unUnsignedInt y)
  setConditionFlag (conditionToFlag (a' == 0))
  incrementProgramCounter

modulusUnsigned ::
  (HasMachineState m, HasParams m) =>
  Register ->
  Register ->
  ImmediateOrRegister ->
  m ()
modulusUnsigned ri rj a = do
  a' <- UnsignedInt <$> getImmediateOrRegister a
  rj' <- UnsignedInt <$> getRegisterValue rj
  let y = if a' == 0 then 0 else rj' `mod` a'
  setRegisterValue ri (unUnsignedInt y)
  setConditionFlag (conditionToFlag (a' == 0))
  incrementProgramCounter

shiftLeft ::
  (HasMachineState m, HasParams m) =>
  Register ->
  Register ->
  ImmediateOrRegister ->
  m ()
shiftLeft ri rj a = do
  a' <- UnsignedInt <$> getImmediateOrRegister a
  rj' <- getRegisterValue rj
  ws <- getWordSize
  wsb <- getWordSizeBitmask
  setRegisterValue ri $ (rj' `shift` fromIntegral (min (fromIntegral ws) a')) .&. wsb
  setConditionFlag . conditionToFlag $
    (rj' .&. (2 ^ (fromIntegral ws - 1 :: Integer))) /= 0
  incrementProgramCounter

shiftRight ::
  (HasMachineState m, HasParams m) =>
  Register ->
  Register ->
  ImmediateOrRegister ->
  m ()
shiftRight ri rj a = do
  a' <- UnsignedInt <$> getImmediateOrRegister a
  rj' <- getRegisterValue rj
  ws <- getWordSize
  setRegisterValue ri $ rj' `shift` fromIntegral (negate (min (fromIntegral ws) a'))
  setConditionFlag . Flag . fromIntegral $ rj' .&. 1
  incrementProgramCounter

compareEqual ::
  (HasMachineState m, HasParams m) =>
  Register ->
  ImmediateOrRegister ->
  m ()
compareEqual ri a = do
  a' <- getImmediateOrRegister a
  ri' <- getRegisterValue ri
  setConditionFlag . conditionToFlag $ a' == ri'
  incrementProgramCounter

compareGreaterUnsigned ::
  (HasMachineState m, HasParams m) =>
  Register ->
  ImmediateOrRegister ->
  m ()
compareGreaterUnsigned ri a = do
  a' <- UnsignedInt <$> getImmediateOrRegister a
  ri' <- UnsignedInt <$> getRegisterValue ri
  setConditionFlag . conditionToFlag $ ri' > a'
  incrementProgramCounter

compareGreaterOrEqualUnsigned ::
  (HasMachineState m, HasParams m) =>
  Register ->
  ImmediateOrRegister ->
  m ()
compareGreaterOrEqualUnsigned ri a = do
  a' <- UnsignedInt <$> getImmediateOrRegister a
  ri' <- UnsignedInt <$> getRegisterValue ri
  setConditionFlag . conditionToFlag $ ri' >= a'
  incrementProgramCounter

compareGreaterSigned ::
  (HasMachineState m, HasParams m) =>
  Register ->
  ImmediateOrRegister ->
  m ()
compareGreaterSigned ri a = do
  ws <- getWordSize
  a' <- decodeSignedInt ws . SignedInt <$> getImmediateOrRegister a
  ri' <- decodeSignedInt ws . SignedInt <$> getRegisterValue ri
  setConditionFlag . conditionToFlag $ ri' > a'
  incrementProgramCounter

compareGreaterOrEqualSigned ::
  (HasMachineState m, HasParams m) =>
  Register ->
  ImmediateOrRegister ->
  m ()
compareGreaterOrEqualSigned ri a = do
  ws <- getWordSize
  a' <- decodeSignedInt ws . SignedInt <$> getImmediateOrRegister a
  ri' <- decodeSignedInt ws . SignedInt <$> getRegisterValue ri
  setConditionFlag . conditionToFlag $ ri' >= a'
  incrementProgramCounter

move ::
  (HasMachineState m, HasParams m) =>
  Register ->
  ImmediateOrRegister ->
  m ()
move ri a = do
  a' <- getImmediateOrRegister a
  setRegisterValue ri a'
  incrementProgramCounter

conditionalMove ::
  (HasMachineState m, HasParams m) =>
  Register ->
  ImmediateOrRegister ->
  m ()
conditionalMove ri a = do
  flag <- getConditionFlag
  case flag of
    1 -> move ri a
    _ -> incrementProgramCounter

jump ::
  (HasMachineState m) =>
  ImmediateOrRegister ->
  m ()
jump a = do
  a' <- ProgramCounter . Address <$> getImmediateOrRegister a
  setProgramCounter a'

jumpIfFlag ::
  (HasMachineState m, HasParams m) =>
  ImmediateOrRegister ->
  m ()
jumpIfFlag a = do
  flag <- getConditionFlag
  case flag of
    1 -> jump a
    _ -> incrementProgramCounter

jumpIfNotFlag ::
  (HasMachineState m, HasParams m) =>
  ImmediateOrRegister ->
  m ()
jumpIfNotFlag a = do
  flag <- getConditionFlag
  case flag of
    0 -> jump a
    _ -> incrementProgramCounter

store ::
  (HasMachineState m, HasParams m) =>
  ImmediateOrRegister ->
  Register ->
  m ()
store a ri = do
  a' <- Address <$> getImmediateOrRegister a
  ri' <- getRegisterValue ri
  wordSize <- getWordSize
  let (aAligned, _) = alignToWord wordSize a'
  setWord aAligned ri'
  incrementProgramCounter

storeb ::
  (HasMachineState m, HasParams m) =>
  ImmediateOrRegister ->
  Register ->
  m ()
storeb a ri = do
  a' <- Address <$> getImmediateOrRegister a
  ri' <- getRegisterValue ri
  wordSize <- getWordSize
  let riTrunc :: Integer
      riTrunc = fromIntegral ri' .&. 0xff
      (aAligned, aOffset) = alignToWord wordSize a'
  prevWord <- getWord aAligned
  setWord aAligned (setByte prevWord aOffset riTrunc)
  incrementProgramCounter

load ::
  (HasMachineState m, HasParams m) =>
  Register ->
  ImmediateOrRegister ->
  m ()
load ri a = do
  a' <- Address <$> getImmediateOrRegister a
  wordSize <- getWordSize
  let (aAligned, _) = alignToWord wordSize a'
  v <- getWord aAligned
  setRegisterValue ri v
  incrementProgramCounter

loadb ::
  (HasMachineState m, HasParams m) =>
  Register ->
  ImmediateOrRegister ->
  m ()
loadb ri a = do
  a' <- Address <$> getImmediateOrRegister a
  wordSize <- getWordSize
  let (aAligned, aOffset) = alignToWord wordSize a'
  v <- getWord aAligned
  let b = extractByte v aOffset
  setRegisterValue ri b
  incrementProgramCounter

readInputTape ::
  (HasMachineState m, HasParams m) =>
  Register ->
  ImmediateOrRegister ->
  m ()
readInputTape ri a = do
  a' <- getImmediateOrRegister a
  next <- case a' of
    0 -> readPrimaryInput
    1 -> readAuxiliaryInput
    _ -> return Nothing
  case next of
    Just next' -> do
      setRegisterValue ri next'
      setConditionFlag 0
    Nothing -> do
      setRegisterValue ri 0
      setConditionFlag 1
  incrementProgramCounter

alignToWord :: WordSize -> Address -> (Address, Integer)
alignToWord ws address =
  (address - fromIntegral offset, toInteger offset)
  where
    offset = fromIntegral address `rem` bytesPerWord ws

setByte :: Word -> Integer -> Integer -> Word
setByte word offset val =
  (word .&. mask) .|. val'
  where
    mask :: Word
    mask = complement (0xff `shift` shift')

    val' :: Word
    val' = fromIntegral val `shift` shift'

    shift' :: Int
    shift' = fromInteger $ 8 * offset

extractByte :: Word -> Integer -> Word
extractByte word offset =
  (word `shift` shift') .&. 0xff
  where
    shift' :: Int
    shift' = -(fromInteger $ 8 * offset)
