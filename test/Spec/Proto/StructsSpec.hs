{-# LANGUAGE OverloadedStrings #-}

module Spec.Proto.StructsSpec where

import Prelude hiding (fail)
import Data.Text (Text, pack, unpack)
import Data.Semigroup ((<>))
import Data.Foldable (toList)
import Data.Sequence (fromList)
import Text.ProtocolBuffers.Basic (uFromString, uToString)
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T

import Control.Monad.Resultant
import qualified Generator

import Util.ReSpec
import Util.Protobuf (hexEncode, getStruct, solpb)
import Util.Solidity
import Util.TestGen 
import Gen.Structs (fileDescriptorProto)
import Gen.Structs.Structs
import Gen.Structs.Structs.In1
import Gen.Structs.Structs.In1.In1_1
import Gen.Structs.Structs.In1.In1_2
import Gen.Structs.Structs.In2
import Gen.Structs.Ext


tests :: Spec ()
tests = spec "StructsSpec" 2 $
  let
    msg = Structs
      { listA = fromList
        [ In2
          { uints = fromList
            [ In1_1 { x = 94, y = 38 }
            , In1_1 { x = 72, y = 21 }
            ]
          , strings = fromList
            [ In1_2 { s = uFromString "foo" }
            , In1_2 { s = uFromString "bar" }
            ]
          }
        , In2
          { uints = fromList
            [ In1_1 { x = 37, y = 15 }
            , In1_1 { x = 99, y = 71 }
            ]
          , strings = fromList
            [ In1_2 { s = uFromString "abc" }
            , In1_2 { s = uFromString "xyz" }
            ]
          }
        ]
      , listB = fromList
        [ Ext { z = 56 }
        , Ext { z = 43 }
        ]
      }

    testContractSrc = pack
       $ "contract StructsSpec {                                                      \n"
      ++ "  function testDecode(bytes bs) returns (bool) {                            \n"
      ++ "    StructsCodec.Structs memory x = StructsCodec.decode(bs);                \n"
      ++ "    string memory foo = 'foo';                                              \n"
      ++ "    string memory bar = 'bar';                                              \n"
      ++ "    string memory abc = 'abc';                                              \n"
      ++ "    string memory xyz = 'xyz';                                              \n"
      ++ "    if(x.listA.length            != 2)        throw;                        \n"
      ++ "    if(x.listA[0].uints.length   != 2)        throw;                        \n"
      ++ "    if(x.listA[0].strings.length != 2)        throw;                        \n"
      ++ "    if(x.listA[1].uints.length   != 2)        throw;                        \n"
      ++ "    if(x.listA[1].strings.length != 2)        throw;                        \n"
      ++ "    if(x.listB.length            != 2)        throw;                        \n"
      ++ "    if(x.listA[0].uints[0].x     != 94)       throw;                        \n"
      ++ "    if(x.listA[0].uints[0].y     != 38)       throw;                        \n"
      ++ "    if(x.listA[0].uints[1].x     != 72)       throw;                        \n"
      ++ "    if(x.listA[0].uints[1].y     != 21)       throw;                        \n"
      ++ "    if(!strcmp(x.listA[0].strings[0].s, foo)) throw;                        \n"
      ++ "    if(!strcmp(x.listA[0].strings[1].s, bar)) throw;                        \n"
      ++ "    if(x.listA[1].uints[0].x     != 37)       throw;                        \n"
      ++ "    if(x.listA[1].uints[0].y     != 15)       throw;                        \n"
      ++ "    if(x.listA[1].uints[1].x     != 99)       throw;                        \n"
      ++ "    if(x.listA[1].uints[1].y     != 71)       throw;                        \n"
      ++ "    if(!strcmp(x.listA[1].strings[0].s, abc)) throw;                        \n"
      ++ "    if(!strcmp(x.listA[1].strings[1].s, xyz)) throw;                        \n"
      ++ "    if(x.listB[0].z              != 56)       throw;                        \n"
      ++ "    if(x.listB[1].z              != 43)       throw;                        \n"
      ++ "    return true;                                                            \n"
      ++ "  }                                                                         \n"
      ++ "  function testEncode() returns (bytes) {                                   \n"
      ++ "    StructsCodec.Structs memory x;                                          \n"
      ++ "    x.listA            = new Structs_In2Codec.Structs_In2[](2);             \n"
      ++ "    x.listA[0].uints   = new Structs_In1_In1_1Codec.Structs_In1_In1_1[](2); \n"
      ++ "    x.listA[0].strings = new Structs_In1_In1_2Codec.Structs_In1_In1_2[](2); \n"
      ++ "    x.listA[1].uints   = new Structs_In1_In1_1Codec.Structs_In1_In1_1[](2); \n"
      ++ "    x.listA[1].strings = new Structs_In1_In1_2Codec.Structs_In1_In1_2[](2); \n"
      ++ "    x.listB            = new ExtCodec.Ext[](2);                             \n"
      ++ "    x.listA[0].uints[0].x   = 94;                                           \n"
      ++ "    x.listA[0].uints[0].y   = 38;                                           \n"
      ++ "    x.listA[0].uints[1].x   = 72;                                           \n"
      ++ "    x.listA[0].uints[1].y   = 21;                                           \n"
      ++ "    x.listA[0].strings[0].s = 'foo';                                        \n"
      ++ "    x.listA[0].strings[1].s = 'bar';                                        \n"
      ++ "    x.listA[1].uints[0].x   = 37;                                           \n"
      ++ "    x.listA[1].uints[0].y   = 15;                                           \n"
      ++ "    x.listA[1].uints[1].x   = 99;                                           \n"
      ++ "    x.listA[1].uints[1].y   = 71;                                           \n"
      ++ "    x.listA[1].strings[0].s = 'abc';                                        \n"
      ++ "    x.listA[1].strings[1].s = 'xyz';                                        \n"
      ++ "    x.listB[0].z            = 56;                                           \n"
      ++ "    x.listB[1].z            = 43;                                           \n"
      ++ "    bytes memory bs = StructsCodec.encode(x);                               \n"
      ++ "    return bs;                                                              \n" 
      ++ "  }                                                                         \n"
      ++ "  function strcmp(string a, string b) returns (bool) {                      \n"
      ++ "    bytes memory aa = bytes(a);                                             \n"
      ++ "    bytes memory bb = bytes(b);                                             \n"
      ++ "    for(uint i=0; i<aa.length; i++)                                         \n"
      ++ "      if(aa[i] != bb[i]) return false;                                      \n"
      ++ "    return true;                                                            \n"
      ++ "  }                                                                         \n"
      ++ "}                                                                           \n"

    encoded = hexEncode msg

  in do
    (struct, structs) <- getStruct fileDescriptorProto "Structs"
    libSrc            <- solpb $ Generator.generate structs
    contract          <- compile "StructsSpec" $ testContractSrc <> libSrc

    test "Decode a message with a complex nested structure" $ do
      runEVM contract $ callDataWithBytes "testDecode" encoded
      return ()

    test "Encode a message with a complex nested structure" $ do
      encodeOutput <- runEVM contract $ callDataNoArgs "testEncode"
      let result = extractReturnedBytes encodeOutput
      if result /= encoded
      then fail . unpack $ "Encoding error. Expected " <> encoded <> " but got " <> result
      else return ()


