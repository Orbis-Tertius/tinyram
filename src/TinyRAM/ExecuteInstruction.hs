{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.ExecuteInstruction (executeInstruction) where

import TinyRAM.Instructions
  ( addUnsigned,
    andBits,
    compareEqual,
    compareGreaterOrEqualSigned,
    compareGreaterOrEqualUnsigned,
    compareGreaterSigned,
    compareGreaterUnsigned,
    conditionalMove,
    divideUnsigned,
    jump,
    jumpIfFlag,
    jumpIfNotFlag,
    load,
    loadb,
    modulusUnsigned,
    move,
    multiplySignedMSB,
    multiplyUnsignedLSB,
    multiplyUnsignedMSB,
    notBits,
    orBits,
    readInputTape,
    shiftLeft,
    shiftRight,
    store,
    storeb,
    subtractUnsigned,
    xorBits,
  )
import TinyRAM.Prelude
import TinyRAM.Types.HasMachineState (HasMachineState (..))
import TinyRAM.Types.HasParams (HasParams)
import TinyRAM.Types.Instruction (Instruction (..))

executeInstruction ::
  (HasMachineState m, HasParams m) =>
  Instruction ->
  m ()
executeInstruction i = case i of
  And ri rj a -> andBits ri rj a
  Or ri rj a -> orBits ri rj a
  Xor ri rj a -> xorBits ri rj a
  Not ri a -> notBits ri a
  Add ri rj a -> addUnsigned ri rj a
  Sub ri rj a -> subtractUnsigned ri rj a
  Mull ri rj a -> multiplyUnsignedLSB ri rj a
  Umulh ri rj a -> multiplyUnsignedMSB ri rj a
  Smulh ri rj a -> multiplySignedMSB ri rj a
  Udiv ri rj a -> divideUnsigned ri rj a
  Umod ri rj a -> modulusUnsigned ri rj a
  Shl ri rj a -> shiftLeft ri rj a
  Shr ri rj a -> shiftRight ri rj a
  Cmpe ri a -> compareEqual ri a
  Cmpa ri a -> compareGreaterUnsigned ri a
  Cmpae ri a -> compareGreaterOrEqualUnsigned ri a
  Cmpg ri a -> compareGreaterSigned ri a
  Cmpge ri a -> compareGreaterOrEqualSigned ri a
  Mov ri a -> move ri a
  Cmov ri a -> conditionalMove ri a
  Jmp a -> jump a
  Cjmp a -> jumpIfFlag a
  Cnjmp a -> jumpIfNotFlag a
  Storeb a ri -> storeb a ri
  Loadb ri a -> loadb ri a
  Storew a ri -> store a ri
  Loadw ri a -> load ri a
  Read ri a -> readInputTape ri a
  Answer _ -> return ()
