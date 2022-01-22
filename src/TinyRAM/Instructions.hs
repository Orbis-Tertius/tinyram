{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}


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
  , compareEqual
  , compareGreaterUnsigned
  , compareGreaterOrEqualUnsigned
  , compareGreaterSigned
  , compareGreaterOrEqualSigned
  , move
  , conditionalMove
  , jump
  , jumpIfFlag
  , jumpIfNotFlag
  , store
  , load
  , readInputTape
  ) where


import           TinyRAM.MachineState              (conditionToFlag,
                                                    getImmediateOrRegister,
                                                    incrementProgramCounter)
import           TinyRAM.Params                    (getWordSize,
                                                    getWordSizeBitmask,
                                                    getWordSizeBitmaskMSB)
import           TinyRAM.Prelude
import           TinyRAM.SignedArithmetic          (decodeSignedInt,
                                                    getUnsignedComponent,
                                                    signedMultiplyHigh)
import           TinyRAM.Types.Address             (Address (..))
import           TinyRAM.Types.Flag                (Flag (..))
import           TinyRAM.Types.HasMachineState     (HasMachineState (..))
import           TinyRAM.Types.HasParams           (HasParams)
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister)
import           TinyRAM.Types.ProgramCounter      (ProgramCounter (..))
import           TinyRAM.Types.Register            (Register)
import           TinyRAM.Types.SignedInt           (SignedInt (..))
import           TinyRAM.Types.UnsignedInt         (UnsignedInt (..))
import           TinyRAM.Types.WordSize            (WordSize (..))


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
      incrementProgramCounter
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
      incrementProgramCounter
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
      incrementProgramCounter
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
      incrementProgramCounter
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
      incrementProgramCounter
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
      incrementProgramCounter
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
      incrementProgramCounter
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
      incrementProgramCounter
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
      let aAbs    = getUnsignedComponent ws a''
          rjAbs   = getUnsignedComponent ws rj''
          yAbs    = aAbs * rjAbs
      setRegisterValue ri . unSignedInt
        $ signedMultiplyHigh ws a'' rj''
      setConditionFlag (conditionToFlag (unUnsignedInt yAbs .&. msb /= 0))
      incrementProgramCounter
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
      incrementProgramCounter
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
      incrementProgramCounter
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
      setRegisterValue ri $ (rj'' `shift` fromIntegral (min (fromIntegral ws) a'')) .&. wsb
      setConditionFlag . conditionToFlag
        $ (rj'' .&. (2 ^ (fromIntegral ws - 1 :: Integer))) /= 0
      incrementProgramCounter
    _ -> return ()


shiftRight :: ( Monad m, HasMachineState m, HasParams m )
  => Register -> Register -> ImmediateOrRegister -> m ()
shiftRight ri rj a = do
  a'  <- UnsignedInt <$$> getImmediateOrRegister a
  rj' <- getRegisterValue rj
  ws  <- getWordSize
  case (a', rj') of
    (Just a'', Just rj'') -> do
      setRegisterValue ri $ rj'' `shift` fromIntegral (negate (min (fromIntegral ws) a''))
      setConditionFlag . Flag . fromIntegral $ rj'' .&. 1
      incrementProgramCounter
    _ -> return ()


compareEqual :: ( Monad m, HasMachineState m )
  => Register -> ImmediateOrRegister -> m ()
compareEqual ri a = do
  a'  <- getImmediateOrRegister a
  ri' <- getRegisterValue ri
  case (a', ri') of
    (Just a'', Just ri'') -> do
      setConditionFlag . conditionToFlag $ a'' == ri''
      incrementProgramCounter
    _ -> return ()


compareGreaterUnsigned :: ( Monad m, HasMachineState m )
  => Register -> ImmediateOrRegister -> m ()
compareGreaterUnsigned ri a = do
  a'  <- UnsignedInt <$$> getImmediateOrRegister a
  ri' <- UnsignedInt <$$> getRegisterValue ri
  case (a', ri') of
    (Just a'', Just ri'') -> do
      setConditionFlag . conditionToFlag $ ri'' > a''
      incrementProgramCounter
    _ -> return ()


compareGreaterOrEqualUnsigned :: ( Monad m, HasMachineState m )
  => Register -> ImmediateOrRegister -> m ()
compareGreaterOrEqualUnsigned ri a = do
  a'  <- UnsignedInt <$$> getImmediateOrRegister a
  ri' <- UnsignedInt <$$> getRegisterValue ri
  case (a', ri') of
    (Just a'', Just ri'') -> do
      setConditionFlag . conditionToFlag $ ri'' >= a''
      incrementProgramCounter
    _ -> return ()


compareGreaterSigned :: ( Monad m, HasMachineState m, HasParams m )
  => Register -> ImmediateOrRegister -> m ()
compareGreaterSigned ri a = do
  ws  <- getWordSize
  a'  <- (decodeSignedInt ws . SignedInt) <$$> getImmediateOrRegister a
  ri' <- (decodeSignedInt ws . SignedInt) <$$> getRegisterValue ri
  case (a', ri') of
    (Just a'', Just ri'') -> do
      setConditionFlag . conditionToFlag $ ri'' > a''
      incrementProgramCounter
    _ -> return ()


compareGreaterOrEqualSigned :: ( Monad m, HasMachineState m, HasParams m )
  => Register -> ImmediateOrRegister -> m ()
compareGreaterOrEqualSigned ri a = do
  ws  <- getWordSize
  a'  <- (decodeSignedInt ws . SignedInt) <$$> getImmediateOrRegister a
  ri' <- (decodeSignedInt ws . SignedInt) <$$> getRegisterValue ri
  case (a', ri') of
    (Just a'', Just ri'') -> do
      setConditionFlag . conditionToFlag $ ri'' >= a''
      incrementProgramCounter
    _ -> return ()


move :: ( Monad m, HasMachineState m )
  => Register -> ImmediateOrRegister -> m ()
move ri a = do
  a' <- getImmediateOrRegister a
  case a' of
    Just a'' -> do
      setRegisterValue ri a''
      incrementProgramCounter
    _ -> return ()


conditionalMove :: ( Monad m, HasMachineState m )
  => Register -> ImmediateOrRegister -> m ()
conditionalMove ri a = do
  flag <- getConditionFlag
  case flag of
    1 -> move ri a
    _ -> incrementProgramCounter


jump :: ( Monad m, HasMachineState m )
  => ImmediateOrRegister -> m ()
jump a = do
  a' <- (ProgramCounter . Address) <$$> getImmediateOrRegister a
  case a' of
    Just a'' -> setProgramCounter a''
    _        -> return ()


jumpIfFlag :: ( Monad m, HasMachineState m )
  => ImmediateOrRegister -> m ()
jumpIfFlag a = do
  flag <- getConditionFlag
  case flag of
    1 -> jump a
    _ -> incrementProgramCounter


jumpIfNotFlag :: ( Monad m, HasMachineState m )
  => ImmediateOrRegister -> m ()
jumpIfNotFlag a = do
  flag <- getConditionFlag
  case flag of
    0 -> jump a
    _ -> incrementProgramCounter


store :: ( Monad m, HasMachineState m )
  => ImmediateOrRegister -> Register -> m ()
store a ri = do
  a'  <- Address <$$> getImmediateOrRegister a
  ri' <- getRegisterValue ri
  case (a', ri') of
    (Just a'', Just ri'') -> do
      setMemoryValue a'' ri''
      incrementProgramCounter
    _ -> return ()


load :: ( Monad m, HasMachineState m )
  => Register -> ImmediateOrRegister -> m ()
load ri a = do
  a' <- Address <$$> getImmediateOrRegister a
  case a' of
    Just a'' -> do
      v <- getMemoryValue a''
      setRegisterValue ri v
      incrementProgramCounter
    Nothing -> return ()


readInputTape :: ( Monad m, HasMachineState m )
  => Register -> ImmediateOrRegister -> m ()
readInputTape ri a = do
  a' <- getImmediateOrRegister a
  next <- case a' of
    Just 0 -> readPrimaryInput
    Just 1 -> readAuxiliaryInput
    _      -> return Nothing
  case next of
    Just next' -> do
      setRegisterValue ri next'
      setConditionFlag 0
    Nothing -> do
      setRegisterValue ri 0
      setConditionFlag 1
  incrementProgramCounter
