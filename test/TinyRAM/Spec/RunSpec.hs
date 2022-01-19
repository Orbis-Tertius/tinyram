{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.RunSpec ( spec ) where


import Control.Monad.Trans.State (StateT (runStateT))
import Data.Functor.Identity (Identity (runIdentity))

import TinyRAM.MachineState (validateMachineState, validateWord)
import TinyRAM.Run (run)
import TinyRAM.Spec.Gen (genParamsMachineState)
import TinyRAM.Spec.Prelude
import TinyRAM.Types.MachineState (MachineState)
import TinyRAM.Types.Params (Params)
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


run' :: (Params, MachineState) -> (Maybe Word, (Params, MachineState))
run' = runIdentity . runStateT (unTinyRAMT (run (Just 1000)))
