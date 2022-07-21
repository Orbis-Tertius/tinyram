module TinyRAM.Operations (getOpCode, getOperation, readOpCode) where

import           TinyRAM.Types.Opcode     (Opcode)
import           TinyRAM.Types.Operations (Operations (..))


getOpCode :: Operations -> Opcode
getOpCode op =
  case op of
   AND    -> 0
   OR     -> 1
   XOR    -> 2
   NOT    -> 3
   ADD    -> 4
   SUB    -> 5
   MULL   -> 6
   UMULH  -> 7
   SMULH  -> 8
   UDIV   -> 9
   UMOD   -> 10
   SHL    -> 11
   SHR    -> 12
   CMPE   -> 13
   CMPA   -> 14
   CMPAE  -> 15
   CMPG   -> 16
   CMPGE  -> 17
   MOV    -> 18
   CMOV   -> 19
   JMP    -> 20
   CJMP   -> 21
   CNJMP  -> 22
   STORE  -> 28
   LOAD   -> 29
   READ   -> 30
   ANSWER -> 31


getOperation :: Opcode -> Maybe Operations
getOperation op =
  case op of
    0  -> Just AND
    1  -> Just OR
    2  -> Just XOR
    3  -> Just NOT
    4  -> Just ADD
    5  -> Just SUB
    6  -> Just MULL
    7  -> Just UMULH
    8  -> Just SMULH
    9  -> Just UDIV
    10 -> Just UMOD
    11 -> Just SHL
    12 -> Just SHR
    13 -> Just CMPE
    14 -> Just CMPA
    15 -> Just CMPAE
    16 -> Just CMPG
    17 -> Just CMPGE
    18 -> Just MOV
    19 -> Just CMOV
    20 -> Just JMP
    21 -> Just CJMP
    22 -> Just CNJMP
    28 -> Just STORE
    29 -> Just LOAD
    30 -> Just READ
    31 -> Just ANSWER
    _  -> Nothing


readOpCode :: String -> Operations
readOpCode "and"    = AND
readOpCode "or"     = OR
readOpCode "xor"    = XOR
readOpCode "not"    = NOT
readOpCode "add"    = ADD
readOpCode "sub"    = SUB
readOpCode "mull"   = MULL
readOpCode "umulh"  = UMULH
readOpCode "smulh"  = SMULH
readOpCode "udiv"   = UDIV
readOpCode "umod"   = UMOD
readOpCode "shl"    = SHL
readOpCode "shr"    = SHR
readOpCode "cmpe"   = CMPE
readOpCode "cmpa"   = CMPA
readOpCode "cmpae"  = CMPAE
readOpCode "cmpg"   = CMPG
readOpCode "cmpge"  = CMPGE
readOpCode "mov"    = MOV
readOpCode "cmov"   = CMOV
readOpCode "jmp"    = JMP
readOpCode "cjmp"   = CJMP
readOpCode "cnjmp"  = CNJMP
readOpCode "store"  = STORE
readOpCode "load"   = LOAD
readOpCode "read"   = READ
readOpCode "answer" = ANSWER
readOpCode _        = error "Unknown operation"
