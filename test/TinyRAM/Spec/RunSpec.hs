{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TinyRAM.Spec.RunSpec (spec) where

import Control.Monad (join)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (StateT (runStateT))
import Data.Either.Combinators (rightToMaybe)
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Map as Map
import Test.Syd (expectationFailure)
import TinyRAM.ExecuteInstruction (executeInstruction)
import TinyRAM.MachineState
  ( getImmediateOrRegister,
    validateMachineState,
    validateWord,
  )
import TinyRAM.Run (run)
import TinyRAM.Spec.Gen
  ( genImmediateOrRegister,
    genParamsMachineState,
  )
import TinyRAM.Spec.Prelude
import TinyRAM.Types.HasMachineState (Error)
import TinyRAM.Types.ImmediateOrRegister
import TinyRAM.Types.Instruction (Instruction (..))
import TinyRAM.Types.MachineState (MachineState)
import TinyRAM.Types.MaxSteps (MaxSteps (..))
import TinyRAM.Types.Params (Params)
import TinyRAM.Types.ProgramMemoryValues
import TinyRAM.Types.TinyRAMT (TinyRAMT (..))
import TinyRAM.Types.Word (Word)

executionError :: Error -> IO a
executionError = expectationFailure . show

spec :: Spec
spec = describe "run" $ do
  it "results in a valid answer" $
    forAll genParamsMachineState $ \x@(ps, _state) ->
      let answer = run'' x
       in case answer of
            Nothing -> return ()
            Just answer' -> validateWord "Answer" ps answer' `shouldBe` mempty

  it "does not change the params" $
    forAll genParamsMachineState $ \x@(ps, _state) ->
      let answer = run' x
       in case answer of
            Right (_, (ps', _)) -> ps' `shouldBe` ps
            Left _ -> return ()

  it "results in a valid machine state" $
    forAll genParamsMachineState $ \x@(ps, _state) ->
      let eitherState = snd . snd <$> run' x
       in case eitherState of
            Right s -> validateMachineState ps s `shouldBe` mempty
            Left _ -> return ()

  it "produces the same result when split into multiple runs" $
    forAll genParamsMachineState $ \x ->
      forAll (MaxSteps <$> choose (0, 100)) $ \(i :: MaxSteps) ->
        forAll (MaxSteps <$> choose (0, 100)) $ \(j :: MaxSteps) ->
          let a = runIdentity . runExceptT . runStateT (unTinyRAMT (run (Just (i + j))))
              b = runIdentity . runExceptT . runStateT (unTinyRAMT (run (Just i) >> run (Just j)))
           in a x `shouldBe` b x

  it "executes a single instruction when MaxSteps = 1" $
    forAll genParamsMachineState $ \x ->
      let a = runIdentity . runExceptT . runStateT (unTinyRAMT (run (Just 1))) $ x
          pc = x ^. _2 . #programCounter . #unProgramCounter
          i = x ^. _2 . #programMemoryValues . #unProgramMemoryValues . at pc
       in case i of
            Just (Answer _) -> return ()
            (Just inst) ->
              let b = runIdentity . runExceptT . runStateT (unTinyRAMT (executeInstruction inst)) $ x
               in (snd <$> a) `shouldBe` (snd <$> b)
            Nothing -> return ()

  it "halts on an answer instruction" $
    forAll genParamsMachineState $ \(ps, _state) ->
      forAll (genImmediateOrRegister (ps ^. #wordSize) (ps ^. #registerCount)) $
        \(a :: ImmediateOrRegister) ->
          let m :: TinyRAMT Identity (Maybe Word, Maybe Word)
              m = do
                expectedAnswer' <- getImmediateOrRegister a
                answer' <- run (Just 100)
                return (answer', Just expectedAnswer')
              pc = _state ^. #programCounter . #unProgramCounter
              updated = Map.insert pc (Answer a) $ _state ^. #programMemoryValues . #unProgramMemoryValues
              s' = #programMemoryValues .~ ProgramMemoryValues updated $ _state
              result = fst <$> (runIdentity . runExceptT . runStateT (unTinyRAMT m) $ (ps, s'))
           in case result of
                Right (answer, expectedAnswer) -> answer `shouldBe` expectedAnswer
                Left e -> executionError e

run' :: (Params, MachineState) -> Either Error (Maybe Word, (Params, MachineState))
run' = runIdentity . runExceptT . runStateT (unTinyRAMT (run (Just 10)))

run'' :: (Params, MachineState) -> Maybe Word
run'' i = join (rightToMaybe (fst <$> run' i))
