module TinyRAM.Types.Operations (Operations(..)) where

data Operations
  = AND
  | OR
  | XOR
  | NOT
  | ADD
  | SUB
  | MULL
  | UMULH
  | SMULH
  | UDIV
  | UMOD
  | SHL
  | SHR
  | CMPE
  | CMPA
  | CMPAE
  | CMPG
  | CMPGE
  | MOV
  | CMOV
  | JMP
  | CJMP
  | CNJMP
  | STORE
  | LOAD
  | READ
  | ANSWER
  deriving (Read, Enum, Eq)
