{-# LANGUAGE OverloadedStrings #-}

module Spec.Proto.SolTypesSpec where

import Prelude hiding (fail)
import Data.Text (Text, pack, unpack)
import Text.ProtocolBuffers.Basic (uFromString, uToString)
import Data.Semigroup ((<>))
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import Control.Monad.Resultant
import qualified Generator

import Util
import Util.ReSpec
import Util.Protobuf (hexEncode, getStruct, solpb)
import Util.Solidity 
import Util.TestGen
import Gen.SolTypes (fileDescriptorProto)
import Gen.SolTypes.SolTypes
import Gen.Solidity.Solidity.Address
import Gen.Solidity.Solidity.Uint
import Gen.Solidity.Solidity.Uint8
import Gen.Solidity.Solidity.Uint16
import Gen.Solidity.Solidity.Uint32
import Gen.Solidity.Solidity.Uint64
import Gen.Solidity.Solidity.Uint128
import Gen.Solidity.Solidity.Uint256
import Gen.Solidity.Solidity.Int
import Gen.Solidity.Solidity.Int8
import Gen.Solidity.Solidity.Int16
import Gen.Solidity.Solidity.Int32
import Gen.Solidity.Solidity.Int64
import Gen.Solidity.Solidity.Int128
import Gen.Solidity.Solidity.Int256
import Gen.Solidity.Solidity.Bytes1  
import Gen.Solidity.Solidity.Bytes2  
import Gen.Solidity.Solidity.Bytes3  
import Gen.Solidity.Solidity.Bytes4  
import Gen.Solidity.Solidity.Bytes5  
import Gen.Solidity.Solidity.Bytes6  
import Gen.Solidity.Solidity.Bytes7  
import Gen.Solidity.Solidity.Bytes8  
import Gen.Solidity.Solidity.Bytes9  
import Gen.Solidity.Solidity.Bytes10 
import Gen.Solidity.Solidity.Bytes11 
import Gen.Solidity.Solidity.Bytes12 
import Gen.Solidity.Solidity.Bytes13 
import Gen.Solidity.Solidity.Bytes14 
import Gen.Solidity.Solidity.Bytes15 
import Gen.Solidity.Solidity.Bytes16 
import Gen.Solidity.Solidity.Bytes17 
import Gen.Solidity.Solidity.Bytes18 
import Gen.Solidity.Solidity.Bytes19 
import Gen.Solidity.Solidity.Bytes20 
import Gen.Solidity.Solidity.Bytes21 
import Gen.Solidity.Solidity.Bytes22 
import Gen.Solidity.Solidity.Bytes23 
import Gen.Solidity.Solidity.Bytes24 
import Gen.Solidity.Solidity.Bytes25 
import Gen.Solidity.Solidity.Bytes26 
import Gen.Solidity.Solidity.Bytes27 
import Gen.Solidity.Solidity.Bytes28 
import Gen.Solidity.Solidity.Bytes29 
import Gen.Solidity.Solidity.Bytes30 
import Gen.Solidity.Solidity.Bytes31 
import Gen.Solidity.Solidity.Bytes32 

tests :: Spec ()
tests = spec "SolTypeSpec" 2 $
  let
    msg :: SolTypes
    msg = SolTypes
     { a    =  Address $ lfromHex "abcdef0123abcdef0123abcdef0123abcdef0123"
     , u    =  Uint    $ lfunroll 32       5798
     , u8   =  Uint8   $ lfunroll  1         24 
     , u16  =  Uint16  $ lfunroll  2        980
     , u32  =  Uint32  $ lfunroll  4      38432
     , u64  =  Uint64  $ lfunroll  8     854352
     , u128 =  Uint128 $ lfunroll 16    4234346
     , u256 =  Uint256 $ lfunroll 32   54367548
     , i8   =  Int8    $ lsunroll  1       (-68)    
     , i16  =  Int16   $ lsunroll  2        119  
     , i32  =  Int32   $ lsunroll  4     (-1209)  
     , i64  =  Int64   $ lsunroll  8     459234
     , i128 =  Int128  $ lsunroll 16  (-6853963)  
     , i256 =  Int256  $ lsunroll 32   19355035
     , b1   =  Bytes1  $ lfromHex "11"
     , b2   =  Bytes2  $ lfromHex "2345" 
     , b3   =  Bytes3  $ lfromHex "6789ab" 
     , b4   =  Bytes4  $ lfromHex "cdef0123" 
     , b5   =  Bytes5  $ lfromHex "456789abcd" 
     , b6   =  Bytes6  $ lfromHex "ef0123456789" 
     , b7   =  Bytes7  $ lfromHex "abcdef01234567" 
     , b8   =  Bytes8  $ lfromHex "89abcdef01234567" 
     , b9   =  Bytes9  $ lfromHex "89abcdef0123456789" 
     , b10  =  Bytes10 $ lfromHex "abcdef0123456789abcd" 
     , b11  =  Bytes11 $ lfromHex "ef0123456789abcdef0123" 
     , b12  =  Bytes12 $ lfromHex "456789abcdef0123456789ab" 
     , b13  =  Bytes13 $ lfromHex "cdef0123456789abcdef012345" 
     , b14  =  Bytes14 $ lfromHex "6789abcdef0123456789abcdef01" 
     , b15  =  Bytes15 $ lfromHex "23456789abcdef0123456789abcdef" 
     , b16  =  Bytes16 $ lfromHex "0123456789abcdef0123456789abcdef" 
     , b17  =  Bytes17 $ lfromHex "0123456789abcdef0123456789abcdef01" 
     , b18  =  Bytes18 $ lfromHex "23456789abcdef0123456789abcdef012345"
     , b19  =  Bytes19 $ lfromHex "6789abcdef0123456789abcdef0123456789ab" 
     , b20  =  Bytes20 $ lfromHex "cdef0123456789abcdef0123456789abcdef0123" 
     , b21  =  Bytes21 $ lfromHex "456789abcdef0123456789abcdef0123456789abcd" 
     , b22  =  Bytes22 $ lfromHex "ef0123456789abcdef0123456789abcdef0123456789" 
     , b23  =  Bytes23 $ lfromHex "abcdef0123456789abcdef0123456789abcdef01234567" 
     , b24  =  Bytes24 $ lfromHex "89abcdef0123456789abcdef0123456789abcdef01234567" 
     , b25  =  Bytes25 $ lfromHex "89abcdef0123456789abcdef0123456789abcdef0123456789" 
     , b26  =  Bytes26 $ lfromHex "abcdef0123456789abcdef0123456789abcdef0123456789abcd" 
     , b27  =  Bytes27 $ lfromHex "ef0123456789abcdef0123456789abcdef0123456789abcdef0123" 
     , b28  =  Bytes28 $ lfromHex "456789abcdef0123456789abcdef0123456789abcdef0123456789ab" 
     , b29  =  Bytes29 $ lfromHex "cdef0123456789abcdef0123456789abcdef0123456789abcdef012345" 
     , b30  =  Bytes30 $ lfromHex "6789abcdef0123456789abcdef0123456789abcdef0123456789abcdef01" 
     , b31  =  Bytes31 $ lfromHex "23456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" 
     , b32  =  Bytes32 $ lfromHex "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
     } 

    checks =
      [ ("a",    SolType "address" "0xabcdef0123abcdef0123abcdef0123abcdef0123")
      , ("u",    SolType "uint"        "5798")
      , ("u8",   SolType "uint8"         "24")
      , ("u16",  SolType "uint16"       "980")
      , ("u32",  SolType "uint32"     "38432")
      , ("u64",  SolType "uint64"    "854352")
      , ("u128", SolType "uint128"  "4234346")
      , ("u256", SolType "uint256" "54367548")
      , ("i8",   SolType "int8"         "-68")
      , ("i16",  SolType "int16"        "119")  
      , ("i32",  SolType "int32"      "-1209")  
      , ("i64",  SolType "int64"     "459234") 
      , ("i128", SolType "int128"  "-6853963")  
      , ("i256", SolType "int256"  "19355035")
      , ("b1",   SolType "bytes1"  "0x11")
      , ("b2",   SolType "bytes2"  "0x2345")
      , ("b3",   SolType "bytes3"  "0x6789ab")
      , ("b4",   SolType "bytes4"  "0xcdef0123")
      , ("b5",   SolType "bytes5"  "0x456789abcd")
      , ("b6",   SolType "bytes6"  "0xef0123456789")
      , ("b7",   SolType "bytes7"  "0xabcdef01234567")
      , ("b8",   SolType "bytes8"  "0x89abcdef01234567")
      , ("b9",   SolType "bytes9"  "0x89abcdef0123456789")
      , ("b10",  SolType "bytes10" "0xabcdef0123456789abcd")
      , ("b11",  SolType "bytes11" "0xef0123456789abcdef0123")
      , ("b12",  SolType "bytes12" "0x456789abcdef0123456789ab")
      , ("b13",  SolType "bytes13" "0xcdef0123456789abcdef012345")
      , ("b14",  SolType "bytes14" "0x6789abcdef0123456789abcdef01")
      , ("b15",  SolType "bytes15" "0x23456789abcdef0123456789abcdef")
      , ("b16",  SolType "bytes16" "0x0123456789abcdef0123456789abcdef")
      , ("b17",  SolType "bytes17" "0x0123456789abcdef0123456789abcdef01")
      , ("b18",  SolType "bytes18" "0x23456789abcdef0123456789abcdef012345")
      , ("b19",  SolType "bytes19" "0x6789abcdef0123456789abcdef0123456789ab")
      , ("b20",  SolType "bytes20" "0xcdef0123456789abcdef0123456789abcdef0123")
      , ("b21",  SolType "bytes21" "0x456789abcdef0123456789abcdef0123456789abcd")
      , ("b22",  SolType "bytes22" "0xef0123456789abcdef0123456789abcdef0123456789")
      , ("b23",  SolType "bytes23" "0xabcdef0123456789abcdef0123456789abcdef01234567")
      , ("b24",  SolType "bytes24" "0x89abcdef0123456789abcdef0123456789abcdef01234567")
      , ("b25",  SolType "bytes25" "0x89abcdef0123456789abcdef0123456789abcdef0123456789")
      , ("b26",  SolType "bytes26" "0xabcdef0123456789abcdef0123456789abcdef0123456789abcd")
      , ("b27",  SolType "bytes27" "0xef0123456789abcdef0123456789abcdef0123456789abcdef0123")
      , ("b28",  SolType "bytes28" "0x456789abcdef0123456789abcdef0123456789abcdef0123456789ab")
      , ("b29",  SolType "bytes29" "0xcdef0123456789abcdef0123456789abcdef0123456789abcdef012345")
      , ("b30",  SolType "bytes30" "0x6789abcdef0123456789abcdef0123456789abcdef0123456789abcdef01")
      , ("b31",  SolType "bytes31" "0x23456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
      , ("b32",  SolType "bytes32" "0x0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
      ]

    encoded = hexEncode msg

  in do
    (struct, structs) <- getStruct fileDescriptorProto "SolTypes"
    libSrc            <- solpb $ Generator.generate structs
    testContractSrc   <- generateTestContract struct checks
    let code = testContractSrc <> libSrc
    contract      <- compile "SolTypesSpec" code

    test "Decode a message containing all solidity native types" $ do
      runEVM contract $ callDataWithBytes "testDecode" encoded
      return ()

    test "Encode a message containing all solidity native types" $ do
      encodeOutput <- runEVM contract $ callDataNoArgs "testEncode"
      let result = extractReturnedBytes encodeOutput
      if result /= encoded
      then fail . unpack $ "Encoding error. Expected " <> encoded <> " but got " <> result
      else return ()


