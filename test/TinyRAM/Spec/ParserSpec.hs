module TinyRAM.Spec.ParserSpec (spec) where

import           Text.Parsec

import           TinyRAM.Spec.Prelude

import           TinyRAM.Parser              (firstLine, instruction)
import           TinyRAM.Types.RegisterCount (RegisterCount (..))
import           TinyRAM.Types.WordSize      (WordSize (..))

spec :: Spec
spec = describe "parser" $ do
  it "first line of TinyRAM Program" $
    runParser firstLine () "TinyRAM First Line" "; TinyRAM V=1.00 W=8 K=2"
    `shouldBe`
    (Right (WordSize {unWordSize = 8},RegisterCount {unRegisterCount = 2}))
  it "parse instruction without comments" $
    runParser instruction () "TinyRAM instruction" "add r1, r2, 1"
    `shouldBe`
    Right (Just ("add",["r1","r2", "1"]))
  it "parse instruction with comments" $
    runParser instruction () "TinyRAM instruction" "add r1, r2, 1; Just adding comment"
    `shouldBe`
    Right (Just ("add",["r1","r2", "1"]))
  it "parse instruction with label" $
    runParser instruction () "TinyRAM instruction" "lab: add r1, r2, 1"
    `shouldBe`
    Right (Just ("add",["r1","r2", "1"]))
