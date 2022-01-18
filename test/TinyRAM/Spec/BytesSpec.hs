{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TinyRAM.Spec.BytesSpec ( spec ) where


import qualified Data.ByteString as BS

import TinyRAM.Bytes (bytesToWords)
import TinyRAM.Spec.Gen ()
import TinyRAM.Spec.Prelude
import TinyRAM.Types.WordSize (WordSize (..))


spec :: Spec
spec = describe "bytesToWords" $
  it "produces the correct number of words" $
    forAllValid $ \(ws :: WordSize) ->
      forAllValid $ \(bs :: ByteString) ->
        let bytesPerWord = unWordSize ws `quot` 8
        in length (bytesToWords ws bs)
           `shouldBe`
           (BS.length bs `quot` bytesPerWord)
