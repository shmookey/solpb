{-# LANGUAGE OverloadedStrings #-}

module Encode where

import Data.Text (pack, unpack, strip)
import Data.Map (Map)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Map as Map

import Types
import Util


generate :: Struct -> Code
generate struct =
  let
    tmpl = pack
       $ "  // Encoder section                 \n"
      ++ "                                     \n"
      ++ "  $mainEncoder                       \n"
      ++ "  $innerEncoder                      \n"
      ++ "  $nestedEncoder                     \n"
      ++ "  $estimator                         \n"
  in
    format tmpl
      [ ("mainEncoder",     strip $ generateMainEncoder   struct)
      , ("innerEncoder",    strip $ generateInnerEncoder  struct)
      , ("nestedEncoder",   strip $ generateNestedEncoder struct)
      , ("estimator",       strip $ generateEstimator     struct)
      ]

generateMainEncoder :: Struct -> Code
generateMainEncoder struct =
  let
    tmpl = pack
       $ "  function encode($struct r) internal constant returns (bytes) { \n"
      ++ "    bytes memory bs = new bytes(_estimate(r));                   \n"
      ++ "    uint sz = _encode(r, 32, bs);                                \n"
      ++ "    assembly { mstore(bs, sz) }                                  \n"
      ++ "    return bs;                                                   \n"
      ++ "  }                                                              \n"
  in
    format tmpl [("struct", structName struct)]

-- | Construct the encode function
generateInnerEncoder :: Struct -> Code
generateInnerEncoder struct =
  let
    tmpl = pack
       $ "  function _encode($struct r, uint p, bytes bs)        \n"
      ++ "      internal constant returns (uint) {               \n"
      ++ "    uint offset = p;                                   \n"
      ++ "    uint i;                                            \n"
      ++ "    $encoders                                          \n"
      ++ "    return p - offset;                                 \n"
      ++ "  }                                                    \n"

    encoderName :: ValueType -> Code
    encoderName typ = case typ of
      LenDelim (Message (User _ lib)) -> lib <> "._encode_nested"
      _                               -> "_pb._encode_" <> formatEncoding typ

    fieldEncoder :: Field -> Code
    fieldEncoder fld =
      let
        tmpl = pack $
          if
            isRepeated fld
          then
               "    for(i=0; i<r.$field.length; i++) {                \n"
            ++ "      p += _pb._encode_key($key, $wiretype, p, bs);   \n"
            ++ "      p += $encoder(r.$field[i], p, bs);              \n"
            ++ "    }                                                 \n"
          else
               "    p += _pb._encode_key($key, $wiretype, p, bs);     \n"
            ++ "    p += $encoder(r.$key, p, bs);                     \n"
      in
        format tmpl
          [ ("field",    fieldName fld)
          , ("key",      show' $ fieldID fld)
          , ("wiretype", formatWireType $ fieldType fld)
          , ("encoder",  encoderName $ fieldType fld)
          ]

    encoders = mconcat . map fieldEncoder $ structFields struct

  in
    format tmpl
      [ ("struct",   structName struct)
      , ("encoders", strip encoders)
      ]

-- An encoder that begins by writing its own length
generateNestedEncoder :: Struct -> Code
generateNestedEncoder struct =
  let
    tmpl = pack
       $ "  function _encode_nested($struct r, uint p, bytes bs)         \n"
      ++ "      internal constant returns (uint) {                       \n"
      ++ "    uint offset = p;                                           \n"
      ++ "    p += _pb._encode_varint(_estimate(r), p, bs);              \n"
      ++ "    p += _encode(r, p, bs);                                    \n"
      ++ "    return p - offset;                                         \n"
      ++ "  }                                                            \n"
  in
    format tmpl [("struct", structName struct)]


-- | Construct the size estimator function
generateEstimator :: Struct -> Code
generateEstimator struct =
  let
    tmpl = pack
       $ "  function _estimate($struct r) internal constant returns (uint) { \n"
      ++ "    uint e;                                                        \n"
      ++ "    uint i;                                                        \n"
      ++ "    $estimators                                                    \n"
      ++ "    return e;                                                      \n"
      ++ "  }                                                                \n"

    scalarSize :: Field -> Code
    scalarSize fld =
      let
        tmpl = pack $ case fieldType fld of
          VarintType _    -> "_pb._sz_$valtype(r.$field)"
          Fixed64Type _   -> "8"
          Fixed32Type _   -> "4"
          LenDelim ld     -> case ld of
            Bytes              -> "_pb._sz_lendelim(r.$field.length)"
            String             -> "_pb._sz_lendelim(bytes(r.$field).length)"
            Packed             -> error "internal error: packed fields not supported"
            Message (User _ _) -> "_pb._sz_lendelim($lib._estimate(r.$field))"
            Message (Sol t)    -> show $ soltypeSize t
          StartGroup      -> error "internal error: groups not supported"
          EndGroup        -> error "internal error: groups not supported"

        suffix = if isRepeated fld then "[i]" else ""
      in
        format tmpl
          [ ("field",   fieldName fld <> pack suffix)
          , ("lib",     formatLibrary $ fieldType fld)
          , ("valtype", formatEncoding $ fieldType fld)
          ]

    fieldEstimator :: Field -> Code
    fieldEstimator fld =
      let
        tmpl = pack $
          if
            isRepeated fld
          then
            "    for(i=0; i<r.$field.length; i++) e+= $szKey + $szItem; "
          else
             "    e += $keySize + $scalarSize; "
      in
        format tmpl
          [ ("field",  fieldName fld)
          , ("szKey",  if fieldID fld < 16 then "1" else "2")
          , ("szItem", scalarSize fld)
          ]

    estimators = T.unlines . map fieldEstimator $ structFields struct

  in
    format tmpl
      [ ("struct",     structName struct)
      , ("estimators", strip $ estimators)
      ]

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


