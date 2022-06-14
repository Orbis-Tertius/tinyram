{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}


module TinyRAM.Spec.ExecuteInstructionSpec ( spec ) where


import           Control.Monad.Trans.State         (StateT (runStateT))
import           Data.Functor.Identity             (Identity (runIdentity))

import           Control.Monad.Trans.Except        (runExceptT)
import           TinyRAM.Bytes                     (bytesPerWord)
import           TinyRAM.ExecuteInstruction        (executeInstruction)
import           TinyRAM.MachineState              (conditionToFlag)
import           TinyRAM.SignedArithmetic          (decodeSignedInt,
                                                    getUnsignedComponent,
                                                    signedMultiplyHigh)
import           TinyRAM.Spec.Gen                  (genInstruction,
                                                    genParamsMachineState)
import           TinyRAM.Spec.Prelude
import           TinyRAM.Types.Address             (Address (..))
import           TinyRAM.Types.Flag                (Flag (..))
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (IsImmediate, IsRegister))
import           TinyRAM.Types.Instruction         (Instruction)
import           TinyRAM.Types.MachineState        (MachineState)
import           TinyRAM.Types.Params              (Params)
import           TinyRAM.Types.SignedInt           (SignedInt (..))
import           TinyRAM.Types.TinyRAMT            (TinyRAMT (..))
import           TinyRAM.Types.UnsignedInt         (UnsignedInt (..))
import           TinyRAM.Types.Word                (Word)
import           TinyRAM.Types.WordSize            (WordSize (..))


spec :: Spec
spec = describe "executeInstruction" $
  it "implements the proper state transitions for valid inputs" $
    forAll genParamsMachineState $ \x@(ps, state) ->
      forAll (genInstruction (ps ^. #wordSize) (ps ^. #registerCount)) $ \i ->
        let machineState = snd . snd <$> runIdentity (runExceptT (runStateT (unTinyRAMT (executeInstruction i)) x))
            expectedMachineState = instructionStateTransition ps i state
         in machineState `shouldBe` Right expectedMachineState


instructionStateTransition :: Params -> Instruction -> MachineState -> MachineState
instructionStateTransition ps i =
  case i ^. #opcode of
    -- and
    0  -> functionOpcode (.&.) (\x y -> conditionToFlag (x .&. y == 0)) i ws
    -- or
    1  -> functionOpcode (.|.) (\x y -> conditionToFlag (x .|. y == 0)) i ws
    -- xor
    2  -> functionOpcode xor   (\x y -> conditionToFlag (x `xor` y == 0)) i ws
    -- not
    3  -> functionOpcode (\a _ -> (2 ^ ws - 1) `xor` a) (\a _ -> conditionToFlag (complement a == 0)) i ws
    -- add
    4  -> functionOpcode (\x y -> (x + y) .&. wordSizeBitmask)
                         (\x y -> conditionToFlag $ (x + y) .&. wordSizeBitmaskMSB /= 0)
                         i ws
    -- sub
    5  -> functionOpcode (\x y -> (y + wordStrictBound - x) .&. wordSizeBitmask)
                         (\x y -> conditionToFlag ((y + wordStrictBound - x) .&. wordSizeBitmaskMSB == 0))
                         i ws
    -- mull
    6  -> functionOpcode (\x y -> ((x * y) .&. wordSizeBitmask))
                         (\x y -> conditionToFlag ((x * y) .&. wordSizeBitmaskMSB /= 0))
                         i ws
    -- umulh
    7  -> functionOpcode (\x y -> ((x * y) `shift` (negate (ws ^. #unWordSize))))
                         (\x y -> conditionToFlag ((x * y) .&. wordSizeBitmaskMSB /= 0))
                         i ws
    -- smulh
    8  -> functionOpcode (\x y -> unSignedInt $ signedMultiplyHigh ws (SignedInt x) (SignedInt y))
                         (\x y -> conditionToFlag $ (unUnsignedInt ( (getUnsignedComponent ws (SignedInt x))
                                                                    * getUnsignedComponent ws (SignedInt y) )
                                                       .&. wordSizeBitmaskMSB)
                                                     /= 0)
                         i ws
    -- udiv
    9  -> functionOpcode (\x y -> if x == 0 then 0 else (y `div` x) .&. wordSizeBitmask)
                         (\x _ -> conditionToFlag (x == 0))
                         i ws
    -- umod
    10 -> functionOpcode (\x y -> if x == 0 then 0 else (y `mod` x) .&. wordSizeBitmask)
                         (\x _ -> conditionToFlag (x == 0))
                         i ws
    -- shl
    11 -> functionOpcode (\x y -> (y `shift` fromIntegral (min (fromIntegral ws) x)) .&. wordSizeBitmask)
                         (\_ y -> conditionToFlag $ y .&. (2 ^ (ws ^. #unWordSize - 1)) /= 0)
                         i ws
    -- shr
    12 -> functionOpcode (\x y -> (y `shift` negate (fromIntegral (min (fromIntegral ws) x))) .&. wordSizeBitmask)
                         (\_ y -> Flag . fromIntegral $ y .&. 1)
                         i ws
    -- cmpe
    13 -> comparisonOpcode (==) i ws
    -- cmpa
    14 -> comparisonOpcode (<)  i ws
    -- cmpae
    15 -> comparisonOpcode (<=) i ws
    -- cmpg
    16 -> comparisonOpcode (\x y -> decodeSignedInt ws (SignedInt x) < decodeSignedInt ws (SignedInt y)) i ws
    -- cmpge
    17 -> comparisonOpcode (\x y -> decodeSignedInt ws (SignedInt x) <= decodeSignedInt ws (SignedInt y)) i ws
    -- mov
    18 -> incrementPC ws
        . (\s -> #registerValues . #unRegisterValues . at (i ^. #ri)
              .~ Just (getA i s)
              $ s)
    -- cmov
    19 -> incrementPC ws
        . (\s ->
            if s ^. #conditionFlag == 0
            then s
            else #registerValues . #unRegisterValues . at (i ^. #ri)
              .~ Just (getA i s)
              $ s)
    -- jmp
    20 -> \s -> #programCounter . #unProgramCounter . #unAddress .~ getA i s $ s
    -- cjmp
    21 -> \s -> if s ^. #conditionFlag == 0
                then incrementPC ws s
                else #programCounter . #unProgramCounter . #unAddress .~ getA i s $ s
    -- cnjmp
    22 -> \s -> if s ^. #conditionFlag == 1
                then incrementPC ws s
                else #programCounter . #unProgramCounter . #unAddress .~ getA i s $ s
    -- store
    28 -> incrementPC ws
        . (\s -> #memoryValues . #unMemoryValues . at (fst $ alignToWord ws (Address (getA i s)))
              .~ Just (getRI i s)
               $ s)
    -- load
    29 -> incrementPC ws
        . (\s -> #registerValues . #unRegisterValues . at (i ^. #ri)
              .~ Just (fromMaybe 0 (s ^. #memoryValues . #unMemoryValues . at (fst $ alignToWord ws (Address (getA i s)))))
               $ s)
    -- read
    30 -> incrementPC ws . readInputTape i
    _  -> id
  where
    ws :: WordSize
    ws = ps ^. #wordSize

    wordStrictBound :: Word
    wordStrictBound = 2 ^ (ps ^. #wordSize)

    wordSizeBitmask :: Word
    wordSizeBitmask = 2 ^ (ps ^. #wordSize) - 1

    wordSizeBitmaskMSB :: Word
    wordSizeBitmaskMSB = wordSizeBitmask `shift` (ps ^. #wordSize . #unWordSize)


incrementPC :: WordSize -> MachineState -> MachineState
incrementPC ws s = #programCounter .~ ((s ^. #programCounter + fromIntegral (2 * bytesPerWord ws)) `mod` (2 ^ unWordSize ws)) $ s


alignToWord :: WordSize -> Address -> (Address, Integer)
alignToWord ws address =
  (address - fromIntegral offset, toInteger offset)
  where
    offset = fromIntegral address `rem` bytesPerWord ws


functionOpcode
  :: (Word -> Word -> Word)
  -> (Word -> Word -> Flag)
  -> Instruction
  -> WordSize
  -> MachineState
  -> MachineState
functionOpcode f p i ws s =
  incrementPC ws
  .
  (#registerValues . #unRegisterValues . at (i ^. #ri)
     .~ Just (f a rj))
  .
  (#conditionFlag .~ p a rj)
  $
  s
  where a  = getA i s
        rj = getRJ i s


comparisonOpcode
  :: (Word -> Word -> Bool)
  -> Instruction
  -> WordSize
  -> MachineState
  -> MachineState
comparisonOpcode p i ws s =
  incrementPC ws
  .
  (#conditionFlag .~ conditionToFlag (p a ri))
  $
  s
  where a  = getA i s
        ri = getRI i s


getA :: Instruction -> MachineState -> Word
getA i s =
  case i ^. #a of
    IsImmediate x -> x
    IsRegister r ->
      fromMaybe (error "getA failed")
      $ s ^. #registerValues . #unRegisterValues . at r


getRJ :: Instruction -> MachineState -> Word
getRJ i s =
  fromMaybe (error "getRJ failed")
  $ s ^. #registerValues . #unRegisterValues . at (i ^. #rj)


getRI :: Instruction -> MachineState -> Word
getRI i s =
  fromMaybe (error "getRI failed")
  $ s ^. #registerValues . #unRegisterValues . at (i ^. #ri)


readInputTape :: Instruction -> MachineState -> MachineState
readInputTape i s =
  case getA i s of
    0 -> readPrimaryInputTape i s
    1 -> readAuxiliaryInputTape i s
    _ ->
      (#registerValues . #unRegisterValues . at (i ^. #ri) .~ Just 0)
      .
      (#conditionFlag .~ 1)
      $
      s


readPrimaryInputTape :: Instruction -> MachineState -> MachineState
readPrimaryInputTape i s =
  case s ^. #primaryInput . #unInputTape of
    x:xs ->
      (#registerValues . #unRegisterValues . at (i ^. #ri) .~ Just x)
      .
      (#primaryInput . #unInputTape .~ xs)
      .
      (#conditionFlag .~ 0)
      $
      s
    [] ->
      (#registerValues . #unRegisterValues . at (i ^. #ri) .~ Just 0)
      .
      (#conditionFlag .~ 1)
      $
      s


readAuxiliaryInputTape :: Instruction -> MachineState -> MachineState
readAuxiliaryInputTape i s =
  case s ^. #auxiliaryInput . #unInputTape of
    x:xs ->
      (#registerValues . #unRegisterValues . at (i ^. #ri) .~ Just x)
      .
      (#primaryInput . #unInputTape .~ xs)
      .
      (#conditionFlag .~ 0)
      $
      s
    [] ->
      (#registerValues . #unRegisterValues . at (i ^. #ri) .~ Just 0)
      .
      (#conditionFlag .~ 1)
      $
      s
