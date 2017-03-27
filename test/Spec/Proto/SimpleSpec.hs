{-# LANGUAGE OverloadedStrings #-}

module Spec.Proto.SimpleSpec where

import Prelude hiding (fail)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

import Control.Monad.Resultant
import Types
import qualified Generator

import Util.Protobuf (hexEncode, getStruct)
import Util.Solidity (compile, run, callDataWithBytes)
import Util.TestGen (generateTestContract, onFailure)
import Gen.Simple (fileDescriptorProto)
import Gen.Simple.Simple


test :: App ()
test =
  let
    msg      = Simple 42 16 
    callData = callDataWithBytes "testDecode" $ hexEncode msg
    checks   = [("a", "42"), ("b", "16")]
  in do
    safeIO $ putStr "Decode a simple message: "
    (struct, structs) <- getStruct fileDescriptorProto "Simple"
    libSrc            <- Generator.generate structs
    testContractSrc   <- generateTestContract struct checks
    testContract      <- compile "SimpleSpec" (T.append testContractSrc libSrc)
    output            <- recoverWith (onFailure testContractSrc callData (show msg) "SimpleSpec") 
                       $ run testContract callData
    safeIO $ putStrLn "PASS"
    return ()

