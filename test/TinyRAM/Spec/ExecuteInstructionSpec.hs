{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Spec.ExecuteInstructionSpec (spec) where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (StateT (runStateT))
import Data.Functor.Identity (Identity (runIdentity))
import TinyRAM.Bytes (bytesPerWord)
import TinyRAM.ExecuteInstruction (executeInstruction)
import TinyRAM.MachineState (conditionToFlag)
import TinyRAM.SignedArithmetic
  ( decodeSignedInt,
    getUnsignedComponent,
    signedMultiplyHigh,
  )
import TinyRAM.Spec.Gen
  ( genInstruction,
    genParamsMachineState,
  )
import TinyRAM.Spec.Prelude
import TinyRAM.Types.Address (Address (..))
import TinyRAM.Types.Flag (Flag (..))
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (IsImmediate, IsRegister))
import TinyRAM.Types.Instruction (Instruction (..))
import TinyRAM.Types.MachineState (MachineState)
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.Register
import TinyRAM.Types.SignedInt (SignedInt (..))
import TinyRAM.Types.TinyRAMT (TinyRAMT (..))
import TinyRAM.Types.UnsignedInt (UnsignedInt (..))
import TinyRAM.Types.Word (Word)
import TinyRAM.Types.WordSize (WordSize (..))

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
  case i of
    And ri rj a -> functionOpcode (.&.) (\x y -> conditionToFlag (x .&. y == 0)) (ri, rj, a) ws
    Or ri rj a -> functionOpcode (.|.) (\x y -> conditionToFlag (x .|. y == 0)) (ri, rj, a) ws
    Xor ri rj a -> functionOpcode xor (\x y -> conditionToFlag (x `xor` y == 0)) (ri, rj, a) ws
    Not ri a -> functionOpcode1 (\a' -> (2 ^ ws - 1) `xor` a') (\a' -> conditionToFlag (complement a' == 0)) (ri, a) ws
    Add ri rj a ->
      functionOpcode
        (\x y -> (x + y) .&. wordSizeBitmask)
        (\x y -> conditionToFlag $ (x + y) .&. wordSizeBitmaskMSB /= 0)
        (ri, rj, a)
        ws
    Sub ri rj a ->
      functionOpcode
        (\x y -> (y + wordStrictBound - x) .&. wordSizeBitmask)
        (\x y -> conditionToFlag ((y + wordStrictBound - x) .&. wordSizeBitmaskMSB == 0))
        (ri, rj, a)
        ws
    Mull ri rj a ->
      functionOpcode
        (\x y -> (x * y) .&. wordSizeBitmask)
        (\x y -> conditionToFlag ((x * y) .&. wordSizeBitmaskMSB /= 0))
        (ri, rj, a)
        ws
    Umulh ri rj a ->
      functionOpcode
        (\x y -> (x * y) `shift` negate (ws ^. #unWordSize))
        (\x y -> conditionToFlag ((x * y) .&. wordSizeBitmaskMSB /= 0))
        (ri, rj, a)
        ws
    Smulh ri rj a ->
      functionOpcode
        (\x y -> unSignedInt $ signedMultiplyHigh ws (SignedInt x) (SignedInt y))
        ( \x y ->
            conditionToFlag $
              ( unUnsignedInt
                  ( getUnsignedComponent ws (SignedInt x)
                      * getUnsignedComponent ws (SignedInt y)
                  )
                  .&. wordSizeBitmaskMSB
              )
                /= 0
        )
        (ri, rj, a)
        ws
    Udiv ri rj a ->
      functionOpcode
        (\x y -> if x == 0 then 0 else (y `div` x) .&. wordSizeBitmask)
        (\x _ -> conditionToFlag (x == 0))
        (ri, rj, a)
        ws
    Umod ri rj a ->
      functionOpcode
        (\x y -> if x == 0 then 0 else (y `mod` x) .&. wordSizeBitmask)
        (\x _ -> conditionToFlag (x == 0))
        (ri, rj, a)
        ws
    Shl ri rj a ->
      functionOpcode
        (\x y -> (y `shift` fromIntegral (min (fromIntegral ws) x)) .&. wordSizeBitmask)
        (\_ y -> conditionToFlag $ y .&. (2 ^ (ws ^. #unWordSize - 1)) /= 0)
        (ri, rj, a)
        ws
    Shr ri rj a ->
      functionOpcode
        (\x y -> (y `shift` negate (fromIntegral (min (fromIntegral ws) x))) .&. wordSizeBitmask)
        (\_ y -> Flag . fromIntegral $ y .&. 1)
        (ri, rj, a)
        ws
    Cmpe ri a -> comparisonOpcode (==) (ri, a) ws
    Cmpa ri a -> comparisonOpcode (<) (ri, a) ws
    Cmpae ri a -> comparisonOpcode (<=) (ri, a) ws
    Cmpg ri a -> comparisonOpcode (\x y -> decodeSignedInt ws (SignedInt x) < decodeSignedInt ws (SignedInt y)) (ri, a) ws
    Cmpge ri a -> comparisonOpcode (\x y -> decodeSignedInt ws (SignedInt x) <= decodeSignedInt ws (SignedInt y)) (ri, a) ws
    Mov ri a ->
      incrementPC ws
        . ( \s ->
              #registerValues . #unRegisterValues . at ri
                ?~ getA a s
                $ s
          )
    Cmov ri a ->
      incrementPC ws
        . ( \s ->
              if s ^. #conditionFlag == 0
                then s
                else
                  #registerValues . #unRegisterValues . at ri
                    ?~ getA a s
                    $ s
          )
    Jmp a -> \s -> #programCounter . #unProgramCounter . #unAddress .~ getA a s $ s
    Cjmp a -> \s ->
      if s ^. #conditionFlag == 0
        then incrementPC ws s
        else #programCounter . #unProgramCounter . #unAddress .~ getA a s $ s
    Cnjmp a -> \s ->
      if s ^. #conditionFlag == 1
        then incrementPC ws s
        else #programCounter . #unProgramCounter . #unAddress .~ getA a s $ s
    -- store
    Storew a ri ->
      incrementPC ws
        . ( \s ->
              #memoryValues . #unMemoryValues . at (fst $ alignToWord ws (Address (getA a s)))
                ?~ getRI ri s
                $ s
          )
    -- load
    Loadw ri a ->
      incrementPC ws
        . ( \s ->
              #registerValues . #unRegisterValues . at ri
                .~ Just (fromMaybe 0 (s ^. #memoryValues . #unMemoryValues . at (fst $ alignToWord ws (Address (getA a s)))))
                $ s
          )
    -- read
    Read ri a -> incrementPC ws . readInputTape ri a
    _ -> id
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

functionOpcode ::
  (Word -> Word -> Word) ->
  (Word -> Word -> Flag) ->
  (Register, Register, ImmediateOrRegister) ->
  WordSize ->
  MachineState ->
  MachineState
functionOpcode f p (ri, rj, a) ws s =
  incrementPC ws
    . ( #registerValues . #unRegisterValues . at ri
          ?~ f a' rj'
      )
    . (#conditionFlag .~ p a' rj')
    $ s
  where
    a' = getA a s
    rj' = getRJ rj s

functionOpcode1 ::
  (Word -> Word) ->
  (Word -> Flag) ->
  (Register, ImmediateOrRegister) ->
  WordSize ->
  MachineState ->
  MachineState
functionOpcode1 f p (ri, a) ws s =
  incrementPC ws
    . ( #registerValues . #unRegisterValues . at ri
          .~ Just (f a')
      )
    . (#conditionFlag .~ p a')
    $ s
  where
    a' = getA a s

comparisonOpcode ::
  (Word -> Word -> Bool) ->
  (Register, ImmediateOrRegister) ->
  WordSize ->
  MachineState ->
  MachineState
comparisonOpcode p (ri, a) ws s =
  incrementPC ws
    . (#conditionFlag .~ conditionToFlag (p a' ri'))
    $ s
  where
    a' = getA a s
    ri' = getRI ri s

getA :: ImmediateOrRegister -> MachineState -> Word
getA a s =
  case a of
    IsImmediate x -> x
    IsRegister r ->
      fromMaybe (error "getA failed") $
        s ^. #registerValues . #unRegisterValues . at r

getRJ :: Register -> MachineState -> Word
getRJ rj s =
  fromMaybe (error "getRJ failed") $
    s ^. #registerValues . #unRegisterValues . at rj

getRI :: Register -> MachineState -> Word
getRI ri s =
  fromMaybe (error "getRI failed") $
    s ^. #registerValues . #unRegisterValues . at ri

readInputTape :: Register -> ImmediateOrRegister -> MachineState -> MachineState
readInputTape ri a s =
  case getA a s of
    0 -> readPrimaryInputTape ri s
    1 -> readAuxiliaryInputTape ri s
    _ ->
      (#registerValues . #unRegisterValues . at ri ?~ 0)
        . (#conditionFlag .~ 1)
        $ s

readPrimaryInputTape :: Register -> MachineState -> MachineState
readPrimaryInputTape ri s =
  case s ^. #primaryInput . #unInputTape of
    x : xs ->
      (#registerValues . #unRegisterValues . at ri ?~ x)
        . (#primaryInput . #unInputTape .~ xs)
        . (#conditionFlag .~ 0)
        $ s
    [] ->
      (#registerValues . #unRegisterValues . at ri ?~ 0)
        . (#conditionFlag .~ 1)
        $ s

readAuxiliaryInputTape :: Register -> MachineState -> MachineState
readAuxiliaryInputTape ri s =
  case s ^. #auxiliaryInput . #unInputTape of
    x : xs ->
      (#registerValues . #unRegisterValues . at ri .~ Just x)
        . (#primaryInput . #unInputTape .~ xs)
        . (#conditionFlag .~ 0)
        $ s
    [] ->
      (#registerValues . #unRegisterValues . at ri .~ Just 0)
        . (#conditionFlag .~ 1)
        $ s
