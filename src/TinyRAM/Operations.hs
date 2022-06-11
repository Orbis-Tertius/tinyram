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


getOperation :: Opcode -> Operations
getOperation op =
  case op of
    0  -> AND   
    1  -> OR    
    2  -> XOR   
    3  -> NOT   
    4  -> ADD   
    5  -> SUB   
    6  -> MULL  
    7  -> UMULH 
    8  -> SMULH 
    9  -> UDIV  
    10 -> UMOD  
    11 -> SHL   
    12 -> SHR   
    13 -> CMPE  
    14 -> CMPA  
    15 -> CMPAE 
    16 -> CMPG  
    17 -> CMPGE 
    18 -> MOV   
    19 -> CMOV  
    20 -> JMP   
    21 -> CJMP  
    22 -> CNJMP 
    28 -> STORE 
    29 -> LOAD  
    30 -> READ  
    31 -> ANSWER
    _  -> error "malformed opcode"


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
