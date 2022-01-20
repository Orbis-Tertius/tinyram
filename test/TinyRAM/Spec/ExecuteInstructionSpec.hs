{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}


module TinyRAM.Spec.ExecuteInstructionSpec ( spec ) where


import Control.Monad.Trans.State (StateT (runStateT))
import Data.Functor.Identity (Identity (runIdentity))

import TinyRAM.ExecuteInstruction (executeInstruction)
import TinyRAM.MachineState (conditionToFlag)
import TinyRAM.Spec.Gen (genParamsMachineState, genInstruction)
import TinyRAM.Spec.Prelude
import TinyRAM.Types.Flag (Flag)
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (IsImmediate, IsRegister))
import TinyRAM.Types.Instruction (Instruction)
import TinyRAM.Types.MachineState (MachineState)
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.TinyRAMT (TinyRAMT (..))
import TinyRAM.Types.Word (Word)


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
    0 -> functionOpcode (.&.) (\x y -> conditionToFlag (x .&. y == 0)) i
    -- or
    1 -> functionOpcode (.|.) (\x y -> conditionToFlag (x .|. y == 0)) i
    -- xor
    2 -> functionOpcode xor   (\x y -> conditionToFlag (x `xor` y == 0)) i
    -- not
    3 -> functionOpcode (\a _ -> complement a) (\a _ -> conditionToFlag (complement a == 0)) i
    -- add
    4 -> functionOpcode (\x y -> (x + y) .&. wordSizeBitmask)
                        (\x y -> conditionToFlag $ (x + y) .&. wordSizeBitmaskMSB /= 0)
                        i
    -- sub
    5 -> functionOpcode (\x y -> (y + wordStrictBound - x) .&. wordSizeBitmask)
                        (\x y -> conditionToFlag ((y + wordStrictBound - x) .&. wordSizeBitmaskMSB /= 0))
                        i
    -- mull
    6 -> functionOpcode (\x y -> ((x * y) .&. wordSizeBitmask))
                        (\x y -> conditionToFlag ((x * y) .&. wordSizeBitmaskMSB /= 0))
                        i
    -- umulh
    7 -> functionOpcode (\x y -> ((x * y) `shift` (negate (ps ^. #wordSize . #unWordSize))))
                        (\x y -> conditionToFlag ((x * y) .&. wordSizeBitmaskMSB /= 0))
                        i
    _ -> id
  where
    wordStrictBound :: Word
    wordStrictBound = 2 ^ (ps ^. #wordSize)

    wordSizeBitmask :: Word
    wordSizeBitmask = 2 ^ (ps ^. #wordSize) - 1

    wordSizeBitmaskMSB :: Word
    wordSizeBitmaskMSB = wordSizeBitmask `shift` (ps ^. #wordSize . #unWordSize)


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
