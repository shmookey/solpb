{-# LANGUAGE OverloadedStrings #-}

module Encode where

import Data.Text (pack, unpack, strip)
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Map as Map

import Types
import Util


generate :: Generator Code
generate =
  let
    tmpl = pack
       $ "  // Encoder section                 \n"
      ++ "                                     \n"
      ++ "  $mainEncoder                       \n"
      ++ "  $innerEncoder                      \n"
      ++ "  $estimator                         \n"
      ++ "  $pbHelpers                         \n"
  in do
    name         <- getName
    fields       <- getFields
    mainEncoder  <- return $ generateMainEncoder name
    innerEncoder <- return $ generateInnerEncoder name fields
    estimator    <- return $ generateEstimator name fields

    return $ format tmpl
      [ ("mainEncoder",  strip mainEncoder)
      , ("innerEncoder", strip innerEncoder)
      , ("estimator",    strip estimator)
      , ("pbHelpers",    strip pbHelpers)
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
generateInnerEncoder :: Name -> Map Int (Name, FieldType) -> Code
generateInnerEncoder name fields =
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

    encoderName :: FieldType -> Code
    encoderName ft = case ft of
      Prim "uint32" _ -> "_pb_encode_uint32f"
      Prim "int32" _  -> "_pb_encode_int32f"
      Prim "uint64" _ -> "_pb_encode_uint64f"
      Prim "int64" _  -> "_pb_encode_int64f"
      Ref x _         -> T.append "_pb_encode_" x
      User x _        -> T.append (libraryName x) "._encode"
    
    fieldEncoder :: Int -> (Name, FieldType) -> Code
    fieldEncoder i (k, ft) = 
      let
        ctx = [ ("k",       k)
              , ("i",       pack $ show i)
              , ("wt",      wireType ft)
              , ("encoder", encoderName ft)
              ]
      in flip format ctx $ case isRepeated ft of
        False -> pack
           $ "    p += _pb_encode_key($i, $wt, p, bs); \n"
          ++ "    p += $encoder(r.$k, p, bs);          \n"
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
generateEstimator :: Name -> Map Int (Name, FieldType) -> Code
generateEstimator name fields =
  let
    tmpl = pack
       $ "  function _estimate($name r) internal constant returns (uint) { \n"
      ++ "    uint e = $baseEstimate;                                      \n"
      ++ "    uint i;                                                      \n"
      ++ "    $dynamicEstimators                                           \n"
      ++ "    return e;                                                    \n"
      ++ "  }                                                              \n"

    -- | Calculate the statically-known portion of the size added by a field
    baseContribution :: Int -> (Name, FieldType) -> Int
    baseContribution i (k, ft) =
      if isRepeated ft then 0
      else if isStruct ft then keySize i
      else case ft of Prim t _ -> (keySize i) + (primSize t)
                      Ref t _  -> keySize i
   
    dynamicEstimator :: Int -> (Name, FieldType) -> Code
    dynamicEstimator i (k, ft) = case ft of
      Prim t Repeated -> 
        let tmpl = pack "    e += r.$k.length * $sz; \n" 
            sz = pack . show $ (keySize i) + (primSize t)
        in format tmpl [("k", k), ("sz", sz)]

      Ref t Repeated ->
        let 
          tmpl = pack $ "    for(i=0; i<$len; i++)                  \n"
                     ++ "      e += $keysz + _pb_sz_lendelim($len); \n"
          len = T.concat $ case t of 
            "string" -> ["bytes(r.", k, "[i]).length"]
            _        -> ["r.", k, "[i].length"]
        in
          format tmpl 
            [ ("len",   len)
            , ("keysz", pack . show $ keySize i)
            ]

      Ref t _ ->
        let 
          tmpl = pack "    e += _pb_sz_lendelim($len); \n"
          len = T.concat $ case t of 
            "string" -> ["bytes(r.", k, ").length"]
            _        -> ["r.", k, ".length"]
        in
          format tmpl [("len", len)]

      User t Repeated ->
        let tmpl = pack 
               $ "    for(i=0; i<r.$k.length; i++)                              \n"
              ++ "      e += $keysz + _pb_sz_lendelim($lib._estimate(r.$k[i])); \n"
            lib = libraryName t
            keysz = pack $ show (keySize i)
        in format tmpl [("k",k), ("lib", lib), ("keysz", keysz)]

      User t _ ->
        let tmpl = pack $ "    e += _pb_sz_lendelim($lib._estimate(r.$k)); \n"
            lib = libraryName t
        in format tmpl [("k",k), ("lib", lib)]

      _ -> ""
 
    keySize :: Int -> Int
    keySize i = if i < 16 then 1 else 2

    primSize :: Name -> Int
    primSize x = case unpack x of
      "uint32" -> 4
      "uint64" -> 8
      "int32"  -> 4
      "int64"  -> 8

    baseEstimate = 
      Map.foldrWithKey (\k v -> (+) (baseContribution k v)) 0 fields

    dynamicEstimators = 
      Map.foldrWithKey (\k v -> T.append (dynamicEstimator k v)) "" fields

  in
    format tmpl 
      [ ("name",              name)
      , ("baseEstimate",      pack $ show baseEstimate)
      , ("dynamicEstimators", strip dynamicEstimators)
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

