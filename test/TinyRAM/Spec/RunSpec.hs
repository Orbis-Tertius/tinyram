{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.RunSpec ( spec ) where


import Control.Monad.Trans.State (StateT (runStateT))
import Data.Functor.Identity (Identity (runIdentity))

import TinyRAM.MachineState (validateMachineState, validateWord, getImmediateOrRegister)
import TinyRAM.Run (run)
import TinyRAM.Spec.EncodeInstruction (encodeInstruction)
import TinyRAM.Spec.Gen (genParamsMachineState, genImmediateOrRegister)
import TinyRAM.Spec.Prelude
import TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister)
import TinyRAM.Types.Instruction (Instruction (..))
import TinyRAM.Types.HasMachineState (HasMachineState (getProgramCounter, setMemoryValue))
import TinyRAM.Types.MachineState (MachineState)
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.ProgramCounter (ProgramCounter (..))
import TinyRAM.Types.TinyRAMT (TinyRAMT (..))
import TinyRAM.Types.Word (Word)


spec :: Spec
spec = describe "run" $ do
  it "results in a valid answer" $
    forAll genParamsMachineState $ \x@(ps, _state) ->
      let (answer, _) = run' x
      in case answer of
           Nothing -> return ()
           Just answer' -> validateWord "Answer" ps answer' `shouldBe` mempty

  it "does not change the params" $
    forAll genParamsMachineState $ \x@(ps, _state) ->
      let (_, (ps', _)) = run' x
      in ps' `shouldBe` ps

  it "results in a valid machine state" $
    forAll genParamsMachineState $ \x@(ps, _state) ->
      let (_, (_, s)) = run' x
      in validateMachineState ps s `shouldBe` mempty

  it "halts on an answer instruction" $
    forAll genParamsMachineState $ \x@(ps, _state) ->
      forAll (genImmediateOrRegister (ps ^. #wordSize) (ps ^. #registerCount))
        $ \(a :: ImmediateOrRegister) ->
            let ((answer, expectedAnswer), _) = runIdentity (runStateT (unTinyRAMT m) x)
                m = do
                  ProgramCounter pc <- getProgramCounter
                  let (i0, i1) = encodeInstruction (ps ^. #registerCount) (Instruction 31 a 0 0)
                  expectedAnswer' <- getImmediateOrRegister a
                  setMemoryValue pc i0
                  setMemoryValue (pc+1) i1
                  answer' <- run (Just 100)
                  return (answer', expectedAnswer')
            in answer `shouldBe` expectedAnswer


run' :: (Params, MachineState) -> (Maybe Word, (Params, MachineState))
run' = runIdentity . runStateT (unTinyRAMT (run (Just 100)))
