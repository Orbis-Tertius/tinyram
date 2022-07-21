{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.Instruction ( Instruction (..), BinOp(..), UnOp (..), Comp (..) ) where


import           TinyRAM.Prelude
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister)
import           TinyRAM.Types.Register            (Register)


data BinOp = BinOp Register Register ImmediateOrRegister
  deriving (Eq, Ord, Read, Show, Generic)

data UnOp = UnOp Register ImmediateOrRegister
  deriving (Eq, Ord, Read, Show, Generic)

data Comp = Comp Register ImmediateOrRegister
  deriving (Eq, Ord, Read, Show, Generic)


-- by convention the first operand is always a destination operand
data Instruction =
    And    BinOp
  | Or     BinOp
  | Xor    BinOp
  | Not    UnOp
  | Add    BinOp
  | Sub    BinOp
  | Mull   BinOp
  | Umulh  BinOp
  | Smulh  BinOp
  | Udiv   BinOp
  | Umod   BinOp
  | Shl    BinOp
  | Shr    BinOp
  | Cmpe   Comp
  | Cmpa   Comp
  | Cmpae  Comp
  | Cmpg   Comp
  | Cmpge  Comp
  | Mov    UnOp
  | Cmov   UnOp
  | Jmp    ImmediateOrRegister
  | Cjmp   ImmediateOrRegister
  | Cnjmp  ImmediateOrRegister
  | Storeb ImmediateOrRegister Register
  | Loadb  Register ImmediateOrRegister
  | Storew ImmediateOrRegister Register
  | Loadw  Register ImmediateOrRegister
  | Read   Register ImmediateOrRegister
  | Answer ImmediateOrRegister
  deriving (Eq, Ord, Read, Show, Generic)

-- data Instruction =
--   Instruction
--   { opcode :: Opcode
--   , a      :: ImmediateOrRegister
--   , ri     :: Register
--   , rj     :: Register
--   }
--   deriving (Eq, Ord, Read, Show, Generic)
