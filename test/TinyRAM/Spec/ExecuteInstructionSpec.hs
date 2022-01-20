{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}


module TinyRAM.Spec.ExecuteInstructionSpec ( spec ) where


import Control.Monad.Trans.State (StateT (runStateT))
import Data.Functor.Identity (Identity (runIdentity))

import TinyRAM.ExecuteInstruction (executeInstruction)
import TinyRAM.Spec.Gen (genParamsMachineState, genInstruction)
import TinyRAM.Spec.Prelude
import TinyRAM.Types.Instruction (Instruction)
import TinyRAM.Types.MachineState (MachineState)
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.TinyRAMT (TinyRAMT (..))


spec :: Spec
spec = describe "executeInstruction" $
  it "implements the proper state transitions" $
    forAll genParamsMachineState $ \x@(ps, state) ->
      forAll (genInstruction (ps ^. #wordSize) (ps ^. #registerCount)) $ \i ->
        snd (snd (runIdentity (runStateT (unTinyRAMT (executeInstruction i)) x)))
          `shouldBe` instructionStateTransition ps i state


instructionStateTransition :: Params -> Instruction -> MachineState -> MachineState
instructionStateTransition _ _ = id
