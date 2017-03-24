{-# LANGUAGE OverloadedStrings #-}

module Util.Protobuf where

import Prelude hiding (fail)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Base16 as B16

import qualified Text.DescriptorProtos.FileDescriptorProto as FDP
import Text.ProtocolBuffers.WireMessage (Wire, messagePut)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)

import Control.Monad.Resultant
import Types
import qualified Convert

hexEncode :: (ReflectDescriptor msg, Wire msg) => msg -> Text
hexEncode = T.pack . B8.unpack . B16.encode . BL.toStrict . messagePut

getStruct :: FDP.FileDescriptorProto -> Name -> App (Struct, [Struct])
getStruct fdp name = do
  structs <- Convert.collect fdp
  let xs = filter (\(Struct name' _) -> name' == name) structs
  case xs of
    (x:[]) -> return (x, structs)
    []     -> fail $ "couldn't find metadata for message type: " ++ (unpack name)
    _      -> fail $ "ambiguous message type: " ++ (unpack name) 


