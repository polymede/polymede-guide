{-# LANGUAGE Haskell2010                #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Copyright    : (c) 2023 Chris Dornan
-- License      : BSD3
--
-- Maintainer   : Chris Dornan <chris@chrisdornan.com>
-- Stability    : stable
-- Portability  : GHC
--
module PolymedeJSONBase64urlPad(polymedeJSON) where

import qualified Data.ByteString.Lazy.Char8   as LBS
import           Data.Aeson                   as A
import           Data.Multibase.JSON
import           Data.Time
import           GHC.Generics


-- a (file) archive is just a list of entries, one for each file
type Archive = [ArchiveEntry]

-- each entry comprises name, archive time and the file's octets
data ArchiveEntry =
  ArchiveEntry
    { _ae_name :: FilePath
    , _ae_when :: UTCTime
    , _ae_data :: FileContents
    }
  deriving stock (Generic,Show)
  deriving anyclass (FromJSON,ToJSON)

-- we wrap the ByteString contents of the file in a newtype wrapper
-- to make it easy to specify the the JSON instances, down to the 
-- exact multibase codec we want to favour for encoding
newtype FileContents =
  FileContents
    { getFileContents :: LBS.ByteString
    }
  deriving stock (Show)
  deriving newtype (FromByteString,ToByteString)
  deriving (FromJSON,ToJSON) 
    via UsingMultibaseCodec Base64urlPad FileContents

-- to illustrate this we will present the JSON-enccoded test archive and
-- decode it and present and returns the recovered file contents that will, on
-- successful completion, have been round-tripped through the multibase codecs
polymedeJSON :: IO LBS.ByteString
polymedeJSON = do
  putStrLn "------ The JSON archive ------"
  LBS.putStrLn test_archive_json
  putStrLn "------ ---------------- ------"
  case A.decode test_archive_json of
    Just [h,w] -> puts $ extract h <> " " <> extract w
    _ -> fail "failed to decode the archive"

-- the test archive, encoded in JSON
test_archive_json :: LBS.ByteString
test_archive_json = A.encode test_archive

-- a simple illustrative test archive
test_archive :: Archive
test_archive =
  [ ArchiveEntry "hw1.txt" (read "2023-02-02 20:38:00 UTC") $ FileContents "hello"
  , ArchiveEntry "hw2.txt" (read "2023-02-02 20:38:00 UTC") $ FileContents "world"
  ]

-- extracting the file contents from an ArchiveEntry0
extract :: ArchiveEntry -> LBS.ByteString
extract (ArchiveEntry _ _ (FileContents bs)) = bs

-- output its argument to stdout and return it
puts :: LBS.ByteString -> IO LBS.ByteString
puts lbs = lbs <$ LBS.putStrLn lbs
