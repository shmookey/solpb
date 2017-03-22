{-# LANGUAGE OverloadedStrings #-}

module Encode where

import Data.Text (pack, unpack, strip)
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Map as Map

import Types
import Util


generate :: Struct -> Code
generate struct@(Struct name _) =
  let
    tmpl = pack
       $ "  // Encoder section                 \n"
      ++ "                                     \n"
      ++ "  $mainEncoder                       \n"
      ++ "  $innerEncoder                      \n"
      ++ "  $estimator                         \n"
      ++ "  $pbHelpers                         \n"
      ++ "  $soltypeEncoders                   \n"
  in
    format tmpl
      [ ("mainEncoder",     strip $ generateMainEncoder name)
      , ("innerEncoder",    strip $ generateInnerEncoder struct)
      , ("estimator",       strip $ generateEstimator struct)
      , ("pbHelpers",       strip pbHelpers)
      , ("soltypeEncoders", strip soltypeEncoders)
      ]

generateMainEncoder :: Name -> Code
generateMainEncoder name =
  let
    tmpl = pack
       $ "  function encode($name r) internal constant returns (bytes) { \n"
      ++ "    bytes memory bs = new bytes(_estimate(r));                 \n"
      ++ "    uint sz = _encode(r, 32, bs);                              \n"
      ++ "    assembly { mstore(bs, sz) }                                \n"
      ++ "    return bs;                                                 \n"
      ++ "  }                                                            \n"
  in
    format tmpl [("name", name)]

-- | Construct the encode function
generateInnerEncoder :: Struct -> Code
generateInnerEncoder (Struct name fields) =
  let
    tmpl = pack
       $ "  function _encode($name r, uint p, bytes bs)                  \n"
      ++ "      internal constant returns (uint) {                       \n"
      ++ "    uint offset = p;                                           \n"
      ++ "    uint i;                                                    \n"
      ++ "    $fieldEncoders                                             \n"
      ++ "    return p - offset;                                         \n"
      ++ "  }                                                            \n"

    wireType :: FieldType -> Code
    wireType ft = T.append "WireType." $ case ft of
      Prim "uint32" _ -> "Fixed32"
      Prim "int32" _  -> "Fixed32"
      Prim "uint64" _ -> "Fixed64"
      Prim "int64" _  -> "Fixed64"
      Ref _ _         -> "LengthDelim"
      User _ _        -> "LengthDelim"
      Sol _ _         -> "LengthDelim"

    encoderName :: FieldType -> Code
    encoderName ft = case ft of
      Prim "uint32" _ -> "_pb_encode_uint32f"
      Prim "int32" _  -> "_pb_encode_int32f"
      Prim "uint64" _ -> "_pb_encode_uint64f"
      Prim "int64" _  -> "_pb_encode_int64f"
      Ref x _         -> T.append "_pb_encode_" x
      User x _        -> T.append (libraryName x) "._encode"
      Sol x _         -> T.append "_pb_encode_sol_" x
    
    fieldEncoder :: Int -> Field -> Code
    fieldEncoder i (k, ft) = 
      let
        ctx = [ ("k",       k)
              , ("i",       pack $ show i)
              , ("wt",      wireType ft)
              , ("encoder", encoderName ft)
              ]
      in flip format ctx $ case isRepeated ft of
        False -> pack
           $ "    p += _pb_encode_key($i, $wt, p, bs);   \n"
          ++ "    p += $encoder(r.$k, p, bs);            \n"
        True -> pack
           $ "    for(i=0; i<r.$k.length; i++) {         \n"
          ++ "      p += _pb_encode_key($i, $wt, p, bs); \n"
          ++ "      p += $encoder(r.$k[i], p, bs);       \n"
          ++ "    }                                      \n"

    fieldEncoders = Map.foldrWithKey (\k v -> T.append (fieldEncoder k v)) "" fields

  in
    format tmpl 
      [ ("name",          name)
      , ("fieldEncoders", strip fieldEncoders)
      ]

-- | Construct the size estimator function
generateEstimator :: Struct -> Code
generateEstimator (Struct name fields) =
  let
    tmpl = pack
       $ "  function _estimate($name r) internal constant returns (uint) { \n"
      ++ "    uint e;                                                      \n"
      ++ "    uint i;                                                      \n"
      ++ "    $fieldEstimators                                             \n"
      ++ "    return e;                                                    \n"
      ++ "  }                                                              \n"

    scalarSize :: Field -> Code
    scalarSize (k, ft) = 
      let ctx = [ ("k",   k)
                , ("lib", fieldLibrary ft)
                ]
      in flip format ctx . pack $ case ft of
        Prim t _        -> show (primSize t)
        Ref t Repeated  -> "_pb_sz_lendelim(bytes(r.$k[i]).length)"
        Ref t _         -> "_pb_sz_lendelim(bytes(r.$k).length)"
        User t Repeated -> "_pb_sz_lendelim($lib._estimate(r.$k[i]))"
        User t _        -> "_pb_sz_lendelim($lib._estimate(r.$k))"
        Sol t _         -> show (soltypeSize t)

    fieldEstimator :: Int -> Field -> Code
    fieldEstimator i (k, ft) =
      let ctx = [ ("k",          k)
                , ("scalarSize", scalarSize (k, ft))
                , ("keySize",    pack . show $ keySize i)
                ]
      in flip format ctx $ case isRepeated ft of
        False -> pack
           $ "    e += $keySize + $scalarSize;    \n"
        True -> pack
           $ "    for(i=0; i<r.$k.length; i++)    \n"
          ++ "      e += $keySize + $scalarSize;  \n"
 
    keySize :: Int -> Int
    keySize i = if i < 16 then 1 else 2

    primSize :: Name -> Int
    primSize x = case unpack x of
      "uint32" -> 4
      "uint64" -> 8
      "int32"  -> 4
      "int64"  -> 8

    fieldEstimators :: Map Int Field -> Code
    fieldEstimators = Map.foldrWithKey (\k -> T.append . fieldEstimator k) ""

  in
    format tmpl 
      [ ("name",            name)
      , ("fieldEstimators", strip $ fieldEstimators fields)
      ]

pbHelpers :: Code
pbHelpers = pack
   $ "  function _pb_sz_lendelim(uint i) internal constant returns (uint) { \n"
  ++ "    return i + _pb_sz_varint(i);                                      \n"
  ++ "  }                                                                   \n"
  ++ "  function _pb_sz_key(uint i) internal constant returns (uint) {      \n"
  ++ "    if(i < 16) return 1;                                              \n"
  ++ "    else if(i < 2048) return 2;                                       \n"
  ++ "    else if(i < 262144) return 3;                                     \n"
  ++ "    else throw;                                                       \n"
  ++ "  }                                                                   \n"
  ++ "  function _pb_sz_varint(uint i) internal constant returns (uint) {   \n"
  ++ "    if(i < 128) return 1;                                             \n"
  ++ "    else if(i < 16384) return 2;                                      \n"
  ++ "    else if(i < 2097152) return 3;                                    \n"
  ++ "    else if(i < 268435456) return 4;                                  \n"
  ++ "    else throw;                                                       \n"
  ++ "  }                                                                   \n"
  ++ "  function _pb_encode_key(uint i, WireType wt, uint p, bytes bs)      \n"
  ++ "      internal constant returns (uint) {                              \n"
  ++ "  }                                                                   \n"
  ++ "  function _pb_encode_varint(uint i, uint p, bytes bs)                \n"
  ++ "      internal constant returns (uint) {                              \n"
  ++ "  }                                                                   \n"
  ++ "  function _pb_encode_varints(int i, uint p, bytes bs)                \n"
  ++ "      internal constant returns (uint) {                              \n"
  ++ "  }                                                                   \n"
  ++ "  function _pb_encode_bytes(bytes xs, uint p, bytes bs)               \n"
  ++ "      internal constant returns (uint) {                              \n"
  ++ "  }                                                                   \n"
  ++ "  function _pb_encode_string(string xs, uint p, bytes bs)             \n"
  ++ "      internal constant returns (uint) {                              \n"
  ++ "  }                                                                   \n"
  ++ "  function _pb_encode_uint32f(uint32 x, uint p, bytes bs)             \n"
  ++ "      internal constant returns (uint) {                              \n"
  ++ "  }                                                                   \n"
  ++ "  function _pb_encode_uint64f(uint64 x, uint p, bytes bs)             \n"
  ++ "      internal constant returns (uint) {                              \n"
  ++ "  }                                                                   \n"
  ++ "  function _pb_encode_int32f(int32 x, uint p, bytes bs)               \n"
  ++ "      internal constant returns (uint) {                              \n"
  ++ "  }                                                                   \n"
  ++ "  function _pb_encode_int64f(int64 x, uint p, bytes bs)               \n"
  ++ "      internal constant returns (uint) {                              \n"
  ++ "  }                                                                   \n"

soltypeEncoders :: Code
soltypeEncoders = pack
   $ "  function _pb_encode_sol_bytesN(uint n, bytes32 x, uint p, bytes bs)\n"
  ++ "      internal constant returns (uint) {                             \n"
  ++ "    p += _pb_encode_varint(22, p, bs);                               \n"
  ++ "    p += _pb_encode_key(1, WireType.LengthDelim, p, bs);             \n"
  ++ "    p += _pb_encode_varint(20, p, bs);                               \n"
  ++ "    return n + 3;                                                    \n"
  ++ "  }                                                                  \n"
  ++ "  function _pb_encode_sol_address(address x, uint p, bytes bs)       \n"
  ++ "      internal constant returns (uint) {                             \n"
  ++ "      return _pb_encode_sol_bytesN(20, bytes32(x), p, bs);           \n"
  ++ "  }                                                                  \n"
  ++ "  function _pb_encode_sol_uint(uint x, uint p, bytes bs)             \n"
  ++ "      internal constant returns (uint) {                             \n"
  ++ "      return _pb_encode_sol_bytesN(32, bytes32(x), p, bs);           \n"
  ++ "  }                                                                  \n"

soltypeSize :: Name -> Int
soltypeSize x = 3 + case x of -- 3b shell [outerLen, key, innerLen]
  "address" -> 20
  "uint"    -> 32
  "uint8"   -> 1
  "uint16"  -> 2
  "uint32"  -> 4
  "uint64"  -> 8
  "uint128" -> 16
  "uint256" -> 32
  "int"     -> 32
  "int8"    -> 1
  "int16"   -> 2
  "int32"   -> 4
  "int64"   -> 8
  "int128"  -> 16
  "int256"  -> 32
  "bytes1"  -> 1
  "bytes2"  -> 2
  "bytes3"  -> 3
  "bytes4"  -> 4
  "bytes5"  -> 5
  "bytes6"  -> 6
  "bytes7"  -> 7
  "bytes8"  -> 8
  "bytes9"  -> 9
  "bytes10" -> 10
  "bytes11" -> 11
  "bytes12" -> 12
  "bytes13" -> 13
  "bytes14" -> 14
  "bytes15" -> 15
  "bytes16" -> 16
  "bytes17" -> 17
  "bytes18" -> 18
  "bytes19" -> 19
  "bytes20" -> 20
  "bytes21" -> 21
  "bytes22" -> 22
  "bytes23" -> 23
  "bytes24" -> 24
  "bytes25" -> 25
  "bytes26" -> 26
  "bytes27" -> 27
  "bytes28" -> 28
  "bytes29" -> 29
  "bytes30" -> 30
  "bytes31" -> 31
  "bytes32" -> 32


