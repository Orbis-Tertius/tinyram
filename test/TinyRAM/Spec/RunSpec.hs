{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.RunSpec ( spec ) where


import           Control.Monad                     (join)
import           Control.Monad.Trans.Except        (runExceptT)
import           Control.Monad.Trans.State         (StateT (runStateT))
import           Data.Either.Combinators           (rightToMaybe)
import           Data.Functor.Identity             (Identity (runIdentity))

import           Test.Syd                          (expectationFailure)
import           TinyRAM.Bytes                     (bytesPerWord)
import           TinyRAM.DecodeInstruction         (decodeInstruction)
import           TinyRAM.EncodeInstruction         (encodeInstruction)
import           TinyRAM.ExecuteInstruction        (executeInstruction)
import           TinyRAM.MachineState              (getImmediateOrRegister,
                                                    validateMachineState,
                                                    validateWord)
import           TinyRAM.Run                       (run)
import           TinyRAM.Spec.Gen                  (genImmediateOrRegister,
                                                    genParamsMachineState)
import           TinyRAM.Spec.Prelude
import           TinyRAM.Types.HasMachineState     (Error,
                                                    HasMachineState (getProgramCounter, setProgramWord))
import           TinyRAM.Types.HasParams           (HasParams (getParams))
import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister)
import           TinyRAM.Types.Instruction         (Instruction (..))
import           TinyRAM.Types.MachineState        (MachineState)
import           TinyRAM.Types.MaxSteps            (MaxSteps (..))
import           TinyRAM.Types.Params              (Params)
import           TinyRAM.Types.ProgramCounter      (ProgramCounter (..))
import           TinyRAM.Types.TinyRAMT            (TinyRAMT (..))
import           TinyRAM.Types.Word                (Word)
import TinyRAM.Types.WordSize (WordSize (..))

executionError :: Error -> IO a
executionError = expectationFailure . show

spec :: Spec
spec = describe "run" $ do
  it "results in a valid answer" $
    forAll genParamsMachineState $ \x@(ps, _state) ->
      let answer = run'' x
      in case answer of
           Nothing      -> return ()
           Just answer' -> validateWord "Answer" ps answer' `shouldBe` mempty

  it "does not change the params" $
    forAll genParamsMachineState $ \x@(ps, _state) ->
      let answer = run' x
      in  case answer of
        Right (_, (ps', _)) -> ps' `shouldBe` ps
        Left _              -> return ()

  it "results in a valid machine state" $
    forAll genParamsMachineState $ \x@(ps, _state) ->
      let eitherState = snd . snd <$> run' x
      in case eitherState of
        Right s -> validateMachineState ps s `shouldBe` mempty
        Left _  -> return ()

  it "produces the same result when split into multiple runs" $
    forAll genParamsMachineState $ \x ->
      forAll (MaxSteps <$> choose (0, 100)) $ \(i :: MaxSteps) ->
        forAll (MaxSteps <$> choose (0, 100)) $ \(j :: MaxSteps) ->
          let a = runIdentity . runExceptT . runStateT (unTinyRAMT (run (Just (i+j))))
              b = runIdentity . runExceptT . runStateT (unTinyRAMT (run (Just i) >> run (Just j)))
          in a x `shouldBe` b x

  it "executes a single instruction when MaxSteps = 1" $
    forAll genParamsMachineState $ \x ->
      let a  = runIdentity . runExceptT . runStateT  (unTinyRAMT (run (Just 1))) $ x
          ws = x ^. _1 . #wordSize
          pc = x ^. _2 . #programCounter . #unProgramCounter
          i0 = fromMaybe 0 $ x ^. _2 . #programMemoryValues . #unProgramMemoryValues . at pc
          i1 = fromMaybe 0 $ x ^. _2 . #programMemoryValues . #unProgramMemoryValues
               . at ((pc + fromIntegral (bytesPerWord ws)) `mod` (2 ^ unWordSize ws))
          i  = decodeInstruction (x ^. _1 . #wordSize) (x ^. _1 . #registerCount) (i0, i1)
          b  = runIdentity . runExceptT . runStateT (unTinyRAMT (executeInstruction i)) $ x
      in if i ^. #opcode /= 31
         then (snd <$> a) `shouldBe` (snd <$> b)
         else return ()

  it "halts on an answer instruction" $
    forAll genParamsMachineState $ \x@(ps, _state) ->
      forAll (genImmediateOrRegister (ps ^. #wordSize) (ps ^. #registerCount))
        $ \(a :: ImmediateOrRegister) ->
            let m = do
                  ProgramCounter pc <- getProgramCounter
                  wordSize <- (^. #wordSize) <$> getParams
                  let (i0, i1) = encodeInstruction wordSize (ps ^. #registerCount) (Instruction 31 a 0 0)
                  expectedAnswer' <- getImmediateOrRegister a
                  setProgramWord pc i0
                  setProgramWord (pc + fromIntegral (bytesPerWord wordSize)) i1
                  answer' <- run (Just 100)
                  return (answer', Just expectedAnswer')
                result = fst <$> (runIdentity . runExceptT . runStateT (unTinyRAMT m) $ x)
            in case result of
              Right (answer, expectedAnswer) -> answer `shouldBe` expectedAnswer
              Left e                         -> executionError e

run' :: (Params, MachineState) -> Either Error (Maybe Word, (Params, MachineState))
run' = runIdentity . runExceptT . runStateT (unTinyRAMT (run (Just 10)))

run'' :: (Params, MachineState) -> Maybe Word
run'' i = join (rightToMaybe (fst <$> run' i))
