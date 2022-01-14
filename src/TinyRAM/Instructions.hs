{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Instructions
  ( andBits
  , orBits
  , xorBits
  , notBits
  , addUnsigned
  , subtractUnsigned
  , multiplyUnsignedLSB
  , multiplyUnsignedMSB
  , multiplySignedMSB
  , divideUnsigned
  , modulusUnsigned
  , shiftLeft
  , shiftRight
  ) where


import TinyRAM.MachineState (conditionToFlag, getImmediateOrRegister)
import TinyRAM.Params (getWordSize, getWordSizeBitmask, getWordSizeBitmaskMSB)
import TinyRAM.Prelude
import TinyRAM.SignedArithmetic (getSign, getUnsignedComponent)
import TinyRAM.Types.Flag (Flag (..))
import TinyRAM.Types.HasParams (HasParams)
import TinyRAM.Types.HasMachineState (HasMachineState (..))
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister)
import TinyRAM.Types.Register (Register)
import TinyRAM.Types.SignedInt (SignedInt (..))
import TinyRAM.Types.UnsignedInt (UnsignedInt (..))
import TinyRAM.Types.Word (Word)
import TinyRAM.Types.WordSize (WordSize (..))


andBits :: ( Monad m, HasMachineState m )
  => Register -> Register -> ImmediateOrRegister -> m ()
andBits ri rj a = do
  a'  <- getImmediateOrRegister a
  rj' <- getRegisterValue rj
  case (a', rj') of
    (Just a'', Just rj'') -> do
      let y = a'' .&. rj''
      setRegisterValue ri y
      setConditionFlag (conditionToFlag (y == 0))
    _ -> return ()


orBits :: ( Monad m, HasMachineState m )
  => Register -> Register -> ImmediateOrRegister -> m ()
orBits ri rj a = do
  a'  <- getImmediateOrRegister a
  rj' <- getRegisterValue rj
  case (a', rj') of
    (Just a'', Just rj'') -> do
      let y = a'' .|. rj''
      setRegisterValue ri y
      setConditionFlag (conditionToFlag (y == 0))
    _ -> return ()


xorBits :: ( Monad m, HasMachineState m )
  => Register -> Register -> ImmediateOrRegister -> m ()
xorBits ri rj a = do
  a'  <- getImmediateOrRegister a
  rj' <- getRegisterValue rj
  case (a', rj') of
    (Just a'', Just rj'') -> do
      let y = a'' `xor` rj''
      setRegisterValue ri y
      setConditionFlag (conditionToFlag (y == 0))
    _ -> return ()


notBits :: ( Monad m, HasMachineState m )
  => Register -> ImmediateOrRegister -> m ()
notBits ri a = do
  a' <- getImmediateOrRegister a
  case a' of
    Just a'' -> do
      let y = complement a''
      setRegisterValue ri y
      setConditionFlag (conditionToFlag (y == 0))
    Nothing -> return ()


addUnsigned :: ( Monad m, HasMachineState m, HasParams m )
  => Register -> Register -> ImmediateOrRegister -> m ()
addUnsigned ri rj a = do
  a'  <- UnsignedInt <$$> getImmediateOrRegister a
  rj' <- UnsignedInt <$$> getRegisterValue rj
  wsb <- getWordSizeBitmask
  msb <- getWordSizeBitmaskMSB
  case (a', rj') of
    (Just a'', Just rj'') -> do
      let y = a'' + rj''
      setRegisterValue ri (unUnsignedInt y .&. wsb)
      setConditionFlag (conditionToFlag (unUnsignedInt y .&. msb /= 0))
    _ -> return ()


subtractUnsigned :: ( Monad m, HasMachineState m, HasParams m )
  => Register -> Register -> ImmediateOrRegister -> m ()
subtractUnsigned ri rj a = do
  a'  <- UnsignedInt <$$> getImmediateOrRegister a
  rj' <- UnsignedInt <$$> getRegisterValue rj
  ws  <- getWordSize
  wsb <- getWordSizeBitmask
  msb <- getWordSizeBitmaskMSB
  case (a', rj') of
    (Just a'', Just rj'') -> do
      let k = 2 ^ (fromIntegral ws :: UnsignedInt)
          y = rj'' + k - a''
      setRegisterValue ri (unUnsignedInt y .&. wsb)
      setConditionFlag (conditionToFlag (unUnsignedInt y .&. msb /= 0))
    _ -> return ()


multiplyUnsignedLSB :: ( Monad m, HasMachineState m, HasParams m )
  => Register -> Register -> ImmediateOrRegister -> m ()
multiplyUnsignedLSB ri rj a = do
  a'  <- UnsignedInt <$$> getImmediateOrRegister a
  rj' <- UnsignedInt <$$> getRegisterValue rj
  wsb <- getWordSizeBitmask
  msb <- getWordSizeBitmaskMSB
  case (a', rj') of
    (Just a'', Just rj'') -> do
      let y = rj'' * a''
      setRegisterValue ri (unUnsignedInt y .&. wsb)
      setConditionFlag (conditionToFlag (unUnsignedInt y .&. msb /= 0))
    _ -> return ()


multiplyUnsignedMSB :: ( Monad m, HasMachineState m, HasParams m )
  => Register -> Register -> ImmediateOrRegister -> m ()
multiplyUnsignedMSB ri rj a = do
  a'  <- UnsignedInt <$$> getImmediateOrRegister a
  rj' <- UnsignedInt <$$> getRegisterValue rj
  ws  <- getWordSize
  msb <- getWordSizeBitmaskMSB
  case (a', rj') of
    (Just a'', Just rj'') -> do
      let y = rj'' * a''
      setRegisterValue ri (shift (unUnsignedInt y) (negate (unWordSize ws)))
      setConditionFlag (conditionToFlag (unUnsignedInt y .&. msb /= 0))
    _ -> return ()


multiplySignedMSB :: ( Monad m, HasMachineState m, HasParams m )
  => Register -> Register -> ImmediateOrRegister -> m ()
multiplySignedMSB ri rj a = do
  a'  <- SignedInt <$$> getImmediateOrRegister a
  rj' <- SignedInt <$$> getRegisterValue rj
  ws  <- getWordSize
  msb <- getWordSizeBitmaskMSB
  case (a', rj') of
    (Just a'', Just rj'') -> do
      let aSign   = getSign ws a''
          rjSign  = getSign ws rj''
          aAbs    = getUnsignedComponent ws a''
          rjAbs   = getUnsignedComponent ws rj''
          ySign   = aSign * rjSign
          yAbs    = aAbs * rjAbs
          signBit = case ySign of
                      -1 -> 2 ^ (fromIntegral ws - 1 :: Word)
                      _  -> 0
          y        = signBit .&. (shift (unUnsignedInt yAbs) (negate (unWordSize ws)))
      setRegisterValue ri y
      setConditionFlag (conditionToFlag (unUnsignedInt yAbs .&. msb /= 0))
    _ -> return ()


divideUnsigned :: ( Monad m, HasMachineState m )
  => Register -> Register -> ImmediateOrRegister -> m ()
divideUnsigned ri rj a = do
  a'  <- UnsignedInt <$$> getImmediateOrRegister a
  rj' <- UnsignedInt <$$> getRegisterValue rj
  case (a', rj') of
    (Just a'', Just rj'') -> do
      let y = if a'' == 0 then 0 else rj'' `div` a''
      setRegisterValue ri (unUnsignedInt y)
      setConditionFlag (conditionToFlag (a'' == 0))
    _ -> return ()


modulusUnsigned :: ( Monad m, HasMachineState m )
  => Register -> Register -> ImmediateOrRegister -> m ()
modulusUnsigned ri rj a = do
  a'  <- UnsignedInt <$$> getImmediateOrRegister a
  rj' <- UnsignedInt <$$> getRegisterValue rj
  case (a', rj') of
    (Just a'', Just rj'') -> do
      let y = if a'' == 0 then 0 else rj'' `mod` a''
      setRegisterValue ri (unUnsignedInt y)
      setConditionFlag (conditionToFlag (a'' == 0))
    _ -> return ()


shiftLeft :: ( Monad m, HasMachineState m, HasParams m )
  => Register -> Register -> ImmediateOrRegister -> m ()
shiftLeft ri rj a = do
  a'  <- UnsignedInt <$$> getImmediateOrRegister a
  rj' <- getRegisterValue rj
  ws  <- getWordSize
  wsb <- getWordSizeBitmask
  case (a', rj') of
    (Just a'', Just rj'') -> do
      setRegisterValue ri $ (rj'' `shift` (fromIntegral a'')) .&. wsb
      setConditionFlag . Flag . fromIntegral $ rj'' .&. (2 ^ (fromIntegral ws - 1 :: Integer))
    _ -> return ()


shiftRight :: ( Monad m, HasMachineState m )
  => Register -> Register -> ImmediateOrRegister -> m ()
shiftRight ri rj a = do
  a'  <- UnsignedInt <$$> getImmediateOrRegister a
  rj' <- getRegisterValue rj
  case (a', rj') of
    (Just a'', Just rj'') -> do
      setRegisterValue ri $ rj'' `shift` (negate (fromIntegral a''))
      setConditionFlag . Flag . fromIntegral $ rj'' .&. 1
    _ -> return ()
