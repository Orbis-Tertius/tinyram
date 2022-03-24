{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}


module TinyRAM.ExecuteInstruction ( executeInstruction ) where

import           Control.Monad.Except              (throwError)

import           TinyRAM.Instructions              (addUnsigned, andBits,
                                                    compareEqual,
                                                    compareGreaterOrEqualSigned,
                                                    compareGreaterOrEqualUnsigned,
                                                    compareGreaterSigned,
                                                    compareGreaterUnsigned,
                                                    conditionalMove,
                                                    divideUnsigned, jump,
                                                    jumpIfFlag, jumpIfNotFlag,
                                                    load, loadb,
                                                    modulusUnsigned, move,
                                                    multiplySignedMSB,
                                                    multiplyUnsignedLSB,
                                                    multiplyUnsignedMSB,
                                                    notBits, orBits,
                                                    readInputTape, shiftLeft,
                                                    shiftRight, store, storeb,
                                                    subtractUnsigned, xorBits)
import           TinyRAM.Prelude
import           TinyRAM.Types.HasMachineState     (Error (..),
                                                    HasMachineState (..))
import           TinyRAM.Types.HasParams           (HasParams)
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister)
import           TinyRAM.Types.Instruction         (Instruction)
import           TinyRAM.Types.Register            (Register)


executeInstruction :: ( HasMachineState m, HasParams m )
  => Instruction -> m ()
executeInstruction i =
  case i ^. #opcode of
    0  -> threeArgOpcode andBits i
    1  -> threeArgOpcode orBits i
    2  -> threeArgOpcode xorBits i
    3  -> twoArgOpcode notBits i
    4  -> threeArgOpcode addUnsigned i
    5  -> threeArgOpcode subtractUnsigned i
    6  -> threeArgOpcode multiplyUnsignedLSB i
    7  -> threeArgOpcode multiplyUnsignedMSB i
    8  -> threeArgOpcode multiplySignedMSB i
    9  -> threeArgOpcode divideUnsigned i
    10 -> threeArgOpcode modulusUnsigned i
    11 -> threeArgOpcode shiftLeft i
    12 -> threeArgOpcode shiftRight i
    13 -> twoArgOpcode compareEqual i
    14 -> twoArgOpcode compareGreaterUnsigned i
    15 -> twoArgOpcode compareGreaterOrEqualUnsigned i
    16 -> twoArgOpcode compareGreaterSigned i
    17 -> twoArgOpcode compareGreaterOrEqualSigned i
    18 -> twoArgOpcode move i
    19 -> twoArgOpcode conditionalMove i
    20 -> oneArgOpcode jump i
    21 -> oneArgOpcode jumpIfFlag i
    22 -> oneArgOpcode jumpIfNotFlag i
    26 -> twoArgOpcode (flip storeb) i
    27 -> twoArgOpcode loadb i
    28 -> twoArgOpcode (flip store) i
    29 -> twoArgOpcode load i
    30 -> twoArgOpcode readInputTape i
    _  -> throwError InvalidOpcodeError


threeArgOpcode :: (Register -> Register -> ImmediateOrRegister -> a) -> Instruction -> a
threeArgOpcode f i =
  f (i ^. #ri) (i ^. #rj) (i ^. #a)


twoArgOpcode :: (Register -> ImmediateOrRegister -> a) -> Instruction -> a
twoArgOpcode f i = f (i ^. #ri) (i ^. #a)


oneArgOpcode :: (ImmediateOrRegister -> a) -> Instruction -> a
oneArgOpcode f i = f (i ^. #a)
