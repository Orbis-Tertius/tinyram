{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.ExecuteProgram ( executeProgram ) where


import TinyRAM.Prelude
import TinyRAM.Types.InputTape (InputTape, Primary, Auxiliary)
import TinyRAM.Types.MaxSteps (MaxSteps)
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.Program (Program)
import TinyRAM.Types.Word (Word)


executeProgram :: Params
               -> Maybe MaxSteps
               -> Program
               -> InputTape Primary
               -> InputTape Auxiliary
               -> IO (Maybe Word)
executeProgram = todo


todo :: a
todo = todo
