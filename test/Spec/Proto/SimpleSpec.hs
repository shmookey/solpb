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


tests :: Spec ()
tests = spec "SimpleSpec" 2 $
  let
    msg = Simple
      { a = 42
      , b = 16
      }

    checks = 
      [ ("a", Integer . toInteger $ a msg)
      , ("b", Integer . toInteger $ b msg)
      ]

    encoded = hexEncode msg

  in do
    (struct, structs) <- getStruct fileDescriptorProto "Simple"
    libSrc            <- solpb $ Generator.generate structs
    testContractSrc   <- generateTestContract struct checks
    contract          <- compile "SimpleSpec" $ testContractSrc <> libSrc

    test "Decode a simple message" $ do
      runEVM contract $ callDataWithBytes "testDecode" encoded
      return ()

    test "Encode a simple message" $ do
      encodeOutput <- runEVM contract $ callDataNoArgs "testEncode"
      let result = extractReturnedBytes encodeOutput
      if result /= encoded
      then fail . unpack $ "Encoding error. Expected " <> encoded <> " but got " <> result
      else return ()

