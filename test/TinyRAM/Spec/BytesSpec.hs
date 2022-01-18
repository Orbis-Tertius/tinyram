{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Spec.BytesSpec ( spec ) where


import TinyRAM.Spec.Prelude


spec :: Spec
spec = describe "1+1" . it "= 2" $ 1+1 `shouldBe` (2 :: Int)
