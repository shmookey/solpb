{-# LANGUAGE OverloadedStrings #-}

module Spec.Proto.SimpleSpec where

import Prelude hiding (fail)
import Data.Text (Text, pack, unpack)
import Data.Semigroup ((<>))
import qualified Data.Text as T

import Control.Monad.Resultant
import qualified Generator

import Util.ReSpec
import Util.Protobuf (hexEncode, getStruct, solpb)
import Util.Solidity
import Util.TestGen 
import Gen.Simple (fileDescriptorProto)
import Gen.Simple.Simple


test :: Spec ()
test = spec "Decode a simple message" $
  let
    msg = Simple
      { a = 42
      , b = 16
      }

    checks   = 
      [ ("a", Integer . toInteger $ a msg)
      , ("b", Integer . toInteger $ b msg)
      ]

    callData = callDataWithBytes "testDecode" $ hexEncode msg
  in do
    (struct, structs) <- getStruct fileDescriptorProto "Simple"
    libSrc            <- solpb $ Generator.generate structs
    testContractSrc   <- generateTestContract struct checks
    testContract      <- compile "SimpleSpec" $ testContractSrc <> libSrc
    output            <- runEVM testContract callData
    return ()

