{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}


module TinyRAM.Spec.ExecuteInstructionSpec ( spec ) where


import Control.Monad.Trans.State (StateT (runStateT))
import Data.Functor.Identity (Identity (runIdentity))

import TinyRAM.ExecuteInstruction (executeInstruction)
import TinyRAM.MachineState (conditionToFlag)
import TinyRAM.SignedArithmetic (signedMultiplyHigh, getUnsignedComponent, decodeSignedInt)
import TinyRAM.Spec.Gen (genParamsMachineState, genInstruction)
import TinyRAM.Spec.Prelude
import TinyRAM.Types.Flag (Flag)
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (IsImmediate, IsRegister))
import TinyRAM.Types.Instruction (Instruction)
import TinyRAM.Types.MachineState (MachineState)
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.SignedInt (SignedInt (..))
import TinyRAM.Types.TinyRAMT (TinyRAMT (..))
import TinyRAM.Types.UnsignedInt (UnsignedInt (..))
import TinyRAM.Types.Word (Word)
import TinyRAM.Types.WordSize (WordSize)


spec :: Spec
spec = describe "executeInstruction" $
  it "implements the proper state transitions for valid inputs" $
    forAll genParamsMachineState $ \x@(ps, state) ->
      forAll (genInstruction (ps ^. #wordSize) (ps ^. #registerCount)) $ \i ->
        snd (snd (runIdentity (runStateT (unTinyRAMT (executeInstruction i)) x)))
          `shouldBe` instructionStateTransition ps i state


instructionStateTransition :: Params -> Instruction -> MachineState -> MachineState
instructionStateTransition ps i =
  case i ^. #opcode of
    -- and
    0  -> functionOpcode (.&.) (\x y -> conditionToFlag (x .&. y == 0)) i
    -- or
    1  -> functionOpcode (.|.) (\x y -> conditionToFlag (x .|. y == 0)) i
    -- xor
    2  -> functionOpcode xor   (\x y -> conditionToFlag (x `xor` y == 0)) i
    -- not
    3  -> functionOpcode (\a _ -> complement a) (\a _ -> conditionToFlag (complement a == 0)) i
    -- add
    4  -> functionOpcode (\x y -> (x + y) .&. wordSizeBitmask)
                         (\x y -> conditionToFlag $ (x + y) .&. wordSizeBitmaskMSB /= 0)
                         i
    -- sub
    5  -> functionOpcode (\x y -> (y + wordStrictBound - x) .&. wordSizeBitmask)
                         (\x y -> conditionToFlag ((y + wordStrictBound - x) .&. wordSizeBitmaskMSB /= 0))
                         i
    -- mull
    6  -> functionOpcode (\x y -> ((x * y) .&. wordSizeBitmask))
                         (\x y -> conditionToFlag ((x * y) .&. wordSizeBitmaskMSB /= 0))
                         i
    -- umulh
    7  -> functionOpcode (\x y -> ((x * y) `shift` (negate (ws ^. #unWordSize))))
                         (\x y -> conditionToFlag ((x * y) .&. wordSizeBitmaskMSB /= 0))
                         i
    -- smulh
    8  -> functionOpcode (\x y -> unSignedInt $ signedMultiplyHigh ws (SignedInt x) (SignedInt y))
                         (\x y -> conditionToFlag $ (unUnsignedInt ( (getUnsignedComponent ws (SignedInt x))
                                                                    * getUnsignedComponent ws (SignedInt y) )
                                                       .&. wordSizeBitmaskMSB)
                                                     /= 0)
                         i
    -- udiv
    9  -> functionOpcode (\x y -> if x == 0 then 0 else (y `div` x) .&. wordSizeBitmask)
                         (\x _ -> conditionToFlag (x == 0))
                         i
    -- umod
    10 -> functionOpcode (\x y -> if x == 0 then 0 else (y `mod` x) .&. wordSizeBitmask)
                         (\x _ -> conditionToFlag (x == 0))
                         i
    -- shl
    11 -> functionOpcode (\x y -> (y `shift` fromIntegral x) .&. wordSizeBitmask)
                         (\_ y -> conditionToFlag $ y .&. (2 ^ (ws ^. #unWordSize - 1)) /= 0)
                         i
    -- shr
    12 -> functionOpcode (\x y -> (y `shift` negate (fromIntegral x)) .&. wordSizeBitmask)
                         (\_ y -> conditionToFlag $ y .&. 1 /= 0)
                         i
    -- cmpe
    13 -> comparisonOpcode (==) i
    -- cmpa
    14 -> comparisonOpcode (<) i
    -- cmpae
    15 -> comparisonOpcode (<=) i
    -- cmpg
    16 -> comparisonOpcode (\x y -> decodeSignedInt ws (SignedInt x) < decodeSignedInt ws (SignedInt y)) i
    -- cmpge
    17 -> comparisonOpcode (\x y -> decodeSignedInt ws (SignedInt x) <= decodeSignedInt ws (SignedInt y)) i
    -- mov
    18 -> incrementPC
        . (\s -> #registerValues . #unRegisterValues . at (i ^. #ri)
              .~ Just (getA i s)
              $ s)
    -- cmov
    19 -> incrementPC
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
                then incrementPC s
                else #programCounter . #unProgramCounter . #unAddress .~ getA i s $ s
    -- cnjmp
    22 -> \s -> if s ^. #conditionFlag == 1
                then incrementPC s
                else #programCounter . #unProgramCounter . #unAddress .~ getA i s $ s
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

    incrementPC :: MachineState -> MachineState
    incrementPC s = #programCounter .~ (s ^. #programCounter + 1) $ s


functionOpcode
  :: (Word -> Word -> Word)
  -> (Word -> Word -> Flag)
  -> Instruction
  -> MachineState
  -> MachineState
functionOpcode f p i s =
  (#programCounter .~ (s ^. #programCounter + 1))
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
  -> MachineState
  -> MachineState
comparisonOpcode p i s =
  (#programCounter .~ (s ^. #programCounter + 1))
  .
  (#conditionFlag .~ conditionToFlag (p a rj))
  $
  s
  where a  = getA i s
        rj = getRJ i s


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
