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
import           TinyRAM.Types.Instruction         (BinOp (BinOp), Comp (Comp),
                                                    Instruction (..),
                                                    UnOp (UnOp))
import           TinyRAM.Types.Register            (Register)


executeInstruction :: ( HasMachineState m, HasParams m )
  => Instruction -> m ()
executeInstruction i = case i of
  And   (BinOp ri rj a) -> andBits ri rj a
  Or    (BinOp ri rj a) -> orBits ri rj a
  Xor   (BinOp ri rj a) -> xorBits ri rj a
  Not   (UnOp ri a)     -> notBits ri a
  Add   (BinOp ri rj a) -> addUnsigned ri rj a
  Sub   (BinOp ri rj a) -> subtractUnsigned  ri rj a
  Mull  (BinOp ri rj a) -> multiplyUnsignedLSB ri rj a
  Umulh (BinOp ri rj a) -> multiplyUnsignedMSB ri rj a
  Smulh (BinOp ri rj a) -> multiplySignedMSB ri rj a
  Udiv  (BinOp ri rj a) -> divideUnsigned ri rj a
  Umod  (BinOp ri rj a) -> modulusUnsigned ri rj a
  Shl   (BinOp ri rj a) -> shiftLeft ri rj a
  Shr   (BinOp ri rj a) -> shiftRight ri rj a
  Cmpe  (Comp ri a)     -> compareEqual ri a
  Cmpa  (Comp ri a)     -> compareGreaterUnsigned ri a
  Cmpae (Comp ri a)     -> compareGreaterOrEqualUnsigned ri a
  Cmpg  (Comp ri a)     -> compareGreaterSigned ri a
  Cmpge (Comp ri a)     -> compareGreaterOrEqualSigned ri a
  Mov   (UnOp ri a)     -> move ri a
  Cmov  (UnOp ri a)     -> conditionalMove ri a
  Jmp a                 -> jump a
  Cjmp a                -> jumpIfFlag a
  Cnjmp a               -> jumpIfNotFlag a
  Storeb a ri           -> storeb a ri
  Loadb ri a            -> loadb ri a
  Storew a ri           -> store a ri
  Loadw ri a            -> load ri a
  Read ri a             -> readInputTape ri a
  Answer a              -> return ()
