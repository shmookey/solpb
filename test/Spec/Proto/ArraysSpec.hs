{-# LANGUAGE OverloadedStrings #-}

module Spec.Proto.ArraysSpec where

import Prelude hiding (fail)
import Data.Text (Text, pack, unpack)
import Data.Semigroup ((<>))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T

import Control.Monad.Resultant
import qualified Generator

import Util.ReSpec
import Util.Protobuf (hexEncode, getStruct, solpb)
import Util.Solidity
import Util.TestGen 
import Gen.Arrays (fileDescriptorProto)
import Gen.Arrays.Arrays


tests :: Spec ()
tests = spec "ArraysSpec" 2 $
  let
    msg = Arrays
      { primArray = Seq.fromList [42, 96, 1000]
      , refArray = Seq.fromList [BL8.pack "hello", BL8.pack "world"]
      }

    checks = 
      [ ("primArray", IntList "uint32" . map toInteger . toList $ primArray msg)
      , ("refArray", BytesList . toList $ refArray msg)
      ]

    encoded = hexEncode msg

  in do
    (struct, structs) <- getStruct fileDescriptorProto "Arrays"
    libSrc            <- solpb $ Generator.generate structs
    testContractSrc   <- generateTestContract struct checks
    contract          <- compile "ArraysSpec" $ testContractSrc <> libSrc

    test "Decode a message containing repeated elements" $ do
      runEVM contract $ callDataWithBytes "testDecode" encoded
      return ()

    test "Encode a message containing repeated elements" $ do
      encodeOutput <- runEVM contract $ callDataNoArgs "testEncode"
      let result = extractReturnedBytes encodeOutput
      if result /= encoded
      then fail . unpack $ "Encoding error. Expected " <> encoded <> " but got " <> result
      else return ()

