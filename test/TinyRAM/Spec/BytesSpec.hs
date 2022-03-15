{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.BytesSpec ( spec ) where


import           Control.Monad
import qualified Data.ByteString        as BS

import           TinyRAM.Bytes          (bytesToWords, wordsToBytes)
import           TinyRAM.Spec.Gen       ()
import           TinyRAM.Spec.Prelude
import           TinyRAM.Types.WordSize (WordSize (..))

spec :: Spec
spec = describe "bytesToWords" $ do
  it "produces the correct number of words" $
    forAllValid $ \(ws :: WordSize) ->
      forAllValid $ \(bs :: ByteString) ->
        let bytesPerWord = unWordSize ws `quot` 8
        in length (bytesToWords ws bs)
           `shouldBe`
           (BS.length bs `quot` bytesPerWord)
  it "validate identity law, wordsToBytes . bytesToWords = id" $
    forAllValid $ \(ws :: WordSize) ->
      forAllValid $ \(bs :: ByteString) ->
        let bytesPerWord = unWordSize ws `quot` 8
         in when (BS.length bs `rem` bytesPerWord == 0
                 ) $
              let wrds = bytesToWords ws bs
                in wordsToBytes ws wrds
                  `shouldBe`
                  bs
