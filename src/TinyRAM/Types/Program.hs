{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module TinyRAM.Types.Program ( Program (..) ) where


import TinyRAM.Prelude


newtype Program = Program { unProgram :: ByteString }
