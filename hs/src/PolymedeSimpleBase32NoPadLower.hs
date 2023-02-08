{-# LANGUAGE Haskell2010                #-}
{-# LANGUAGE OverloadedStrings          #-}

module PolymedeSimpleBase32NoPadLower(polymedeSimple) where

import qualified Data.ByteString.Char8                    as B
import           Data.Multibase.Pipelines


-- | roundtrip a static test string through the multibase
-- codecs, put it to stdout and return it
polymedeSimple :: IO B.ByteString
polymedeSimple = do
  putStrLn "------ The encoded test string ------"
  print test_input_encoded
  putStrLn "------ ----------------------- ------"
  case decodeMultibase test_input_encoded of
    Right hw -> hw <$ B.putStrLn hw
    Left err -> fail $ "failed with: "++show err

test_input_encoded :: Multibase
test_input_encoded = encodeToMultibaseWith MbBase32NoPadLower test_input

test_input :: B.ByteString
test_input = "hello world"
