{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Types.Instruction (Instruction (..)) where

import TinyRAM.Prelude
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister)
import TinyRAM.Types.Register (Register)

-- by convention the first operand is always a destination operand
data Instruction
  = And Register Register ImmediateOrRegister
  | Or Register Register ImmediateOrRegister
  | Xor Register Register ImmediateOrRegister
  | Not Register ImmediateOrRegister
  | Add Register Register ImmediateOrRegister
  | Sub Register Register ImmediateOrRegister
  | Mull Register Register ImmediateOrRegister
  | Umulh Register Register ImmediateOrRegister
  | Smulh Register Register ImmediateOrRegister
  | Udiv Register Register ImmediateOrRegister
  | Umod Register Register ImmediateOrRegister
  | Shl Register Register ImmediateOrRegister
  | Shr Register Register ImmediateOrRegister
  | Cmpe Register ImmediateOrRegister
  | Cmpa Register ImmediateOrRegister
  | Cmpae Register ImmediateOrRegister
  | Cmpg Register ImmediateOrRegister
  | Cmpge Register ImmediateOrRegister
  | Mov Register ImmediateOrRegister
  | Cmov Register ImmediateOrRegister
  | Jmp ImmediateOrRegister
  | Cjmp ImmediateOrRegister
  | Cnjmp ImmediateOrRegister
  | Storeb ImmediateOrRegister Register
  | Loadb Register ImmediateOrRegister
  | Storew ImmediateOrRegister Register
  | Loadw Register ImmediateOrRegister
  | Read Register ImmediateOrRegister
  | Answer ImmediateOrRegister
  deriving stock (Eq, Ord, Read, Show, Generic)
