{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..),
    load,
  )
where

import Data.ByteString (ByteString)
import qualified Data.Text.IO as T (readFile)
import System.Directory (getHomeDirectory)
import Toml (TomlCodec, TomlDecodeError, (.=))
import qualified Toml (byteString, decode, string)

data Config = Config
  { testStr1 :: String,
    testStr2 :: String
  }

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.string "test_str_2" .= testStr2
    <*> Toml.string "test_str_2" .= testStr2

load :: IO (Either [TomlDecodeError] Config)
load = do
  h <- getHomeDirectory
  c <- T.readFile (h ++ "/.config/hamodoro.toml")
  pure (Toml.decode configCodec c)
