{-# LANGUAGE OverloadedStrings #-}

module Spec.Proto.PrimSpec where

import Prelude hiding (fail)
import Data.Text (Text, pack, unpack)
import Text.ProtocolBuffers.Basic (uFromString, uToString)
import Data.Semigroup ((<>))
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T

import Control.Monad.Resultant
import qualified Generator

import Util
import Util.ReSpec
import Util.Protobuf (hexEncode, getStruct, solpb)
import Util.Solidity (compile, runEVM, callDataWithBytes)
import Util.TestGen
import Gen.Prim (fileDescriptorProto)
import Gen.Prim.Prim

test :: Spec ()
test = spec "Decode a message containing all primitive types" $
  let
    msg :: Prim
    msg = Prim 
     { u32  = 42                   -- uint32
     , u64  = 16                   -- uint64
     , i32  = -12                  -- int32
     , i64  = -1118                -- int64
     , s32  = -15                  -- sint32
     , s64  = -67000               -- sint64
     , f32  = 921                  -- fixed32
     , f64  = 2035                 -- fixed64
     , sf32 = -9325                -- sfixed32
     , sf64 = -34325               -- sfixed64
     , b    = False                -- bool
     , s    = (uFromString "test") -- string
     , bs   = (BL8.pack "foo")     -- bytes
     }

    checks = 
      [ ("u32", Integer . toInteger $ u32 msg)
      , ("u64", Integer . toInteger $ u64 msg)
      , ("i32", Integer . toInteger $ i32 msg)
      , ("i64", Integer . toInteger $ i64 msg)
      , ("s32", Integer . toInteger $ s32 msg)
      , ("s64", Integer . toInteger $ s64 msg)
      , ("f32", Integer . toInteger $ f32 msg)
      , ("f64", Integer . toInteger $ f64 msg)
      , ("sf32",Integer . toInteger $ sf32 msg)
      , ("sf64",Integer . toInteger $ sf64 msg)
      , ("b",   Bool    $ b msg)
      , ("s",   String  . uToString $ s msg)
      , ("bs",  Bytes   $ bs msg)
      ]

    callData = callDataWithBytes "testDecode" $ hexEncode msg
  in do
    (struct, structs) <- getStruct fileDescriptorProto "Prim"
    libSrc            <- solpb $ Generator.generate structs
    testContractSrc   <- generateTestContract struct checks
    
    let code = testContractSrc <> libSrc

    testContract      <- compile "PrimSpec" code
    output            <- runEVM testContract callData

    return ()

