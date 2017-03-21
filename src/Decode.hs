{-# LANGUAGE OverloadedStrings #-}

module Decode where

import qualified Data.Text.Template as Template
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text, pack, strip, stripStart)
import Data.List (nub)

import Gen
import Types
import Util

-- | Name of the decoder function for a given field's scalar type
decoderName :: FieldType -> Name
decoderName ft = T.concat $ case ft of
  User x _ -> ["_decode__", x]
  _        -> ["_pb_decode_", fieldTypeName ft]

-- | Name of the reader function for a field
readerName :: Name -> Name
readerName x = T.concat ["_read_", x]

-- | Name of the counter function for a field
counterName :: Name -> Name
counterName x = T.concat ["_count_", x]

-- | `counterVar i` references field counter `i`
counterVar :: Int -> Name
counterVar i = T.concat ["counts[", pack $ show  (i - 1), "]"]

-- | Determine the type of the field counter structure
counterType :: Gen Name
counterType = (Map.size <$> getFields) >>= \sz ->
  return $ T.concat ["uint[", pack (show sz), "] memory"]

generate :: Gen Code
generate = 
  let
    tmpl = pack
       $ "  // Decoder section                           \n"
      ++ "                                               \n"
      ++ "  $mainDecoder                                 \n"
      ++ "  $innerDecoder                                \n"
      ++ "  $fieldReaders                                \n"
      ++ "  $structDecoders                              \n"
      ++ "  $pbDecoders                                  \n"
  in do
    fields    <- getFields
    main      <- mainDecoder <$> getName
    inner     <- innerDecoder
    readers   <- T.concat <$> mapM (uncurry fieldReader) (Map.toList fields)
    structDec <- return . T.concat . map structDecoder . nub
                        . map fieldTypeName . snd . unzip . Map.elems
                        $ Map.filter (isStruct . snd) fields
    return $ format tmpl
      [ ("mainDecoder",    stripStart main)
      , ("innerDecoder",   stripStart inner)
      , ("fieldReaders",   stripStart readers)
      , ("structDecoders", stripStart structDec)
      , ("pbDecoders",     stripStart pbDecoders)
      ]

mainDecoder :: Name -> Code
mainDecoder name =
  let
    tmpl = pack
       $ "  function decode(bytes bs) internal constant returns ($name) { \n"
      ++ "    var (x,) = _decode(32, bs, bs.length);                      \n"
      ++ "    return x;                                                   \n"
      ++ "  }                                                             \n"
  in
    format tmpl [("name", name)]

innerDecoder :: Gen Code
innerDecoder =
  let
    tmpl = pack
       $ "  function _decode(uint p, bytes bs, uint sz)                 \n"
      ++ "      internal constant returns ($name, uint) {               \n"
      ++ "    $name memory r;                                           \n"
      ++ "    $ctype counts;                                            \n"
      ++ "    uint fieldId;                                             \n"
      ++ "    WireType wireType;                                        \n"
      ++ "    uint bytesRead;                                           \n"
      ++ "    uint offset = p;                                          \n"
      ++ "    while(p < offset+sz) {                                    \n"
      ++ "      (fieldId, wireType, bytesRead) = _pb_decode_key(p, bs); \n"
      ++ "      p += bytesRead;                                         \n"
      ++ "      $firstPass                                              \n"
      ++ "    }                                                         \n"
      ++ "    p = offset;                                               \n"
      ++ "    $allocators                                               \n"
      ++ "    while(p < offset+sz) {                                    \n"
      ++ "      (fieldId, wireType, bytesRead) = _pb_decode_key(p, bs); \n"
      ++ "      p += bytesRead;                                         \n"
      ++ "      $secondPass                                             \n"
      ++ "    }                                                         \n"
      ++ "    return (r, sz);                                           \n"
      ++ "  }                                                           \n"

    -- | `fieldHandler i ft firstPass` handles a field by ID, type and pass
    fieldHandler :: Bool -> Int -> (Name, FieldType) -> Code
    fieldHandler firstPass i (name, ft) =
      let
        tmpl = pack
           $ "      else if(fieldId == $id)         \n"
          ++ "        p += $reader($args);          \n"

        args = case (fieldLabel ft, firstPass) of
          (Repeated, True)  -> "p, bs, wireType, nil(), counts"
          (Repeated, False) -> "p, bs, wireType, r, counts"
          (_       , True)  -> "p, bs, wireType, r, counts"
          (_       , False) -> "p, bs, wireType, nil(), counts"

        ctx =
          [ ("id",     pack $ show i)
          , ("reader", readerName name)
          , ("args",   args)
          ]
      in
        format tmpl ctx

    -- | Make code to initialise an array field's length from its counter
    arrayAllocator :: Int -> (Name, FieldType) -> Code
    arrayAllocator i (name, ft) = format line ctx
      where line = "    r.$name = new $typ($counter);"
            ctx  = [("name", name), ("typ", typ), ("counter", counterVar i)]
            typ  = fieldType ft

    -- | `handlers True` generates field handlers for the first pass
    handlers :: Bool -> Map Int (Name, FieldType) -> Code
    handlers firstPass = 
        flip T.append "      else throw;"                  -- insert closing 'else'
      . T.drop 5 . T.stripStart                            -- delete leading 'else'
      . T.concat . map (uncurry $ fieldHandler firstPass)
      . Map.toList

    -- | Generate array allocators
    allocators :: Map Int (Name, FieldType) -> Code
    allocators = T.intercalate "\n"
      . map (uncurry arrayAllocator)
      . Map.toList
      . Map.filter (isRepeated . snd)

  in do
    fields <- getFields
    name   <- getName
    ctype  <- counterType
    ctx    <- return
      [ ("name",       name)
      , ("ctype",      ctype)
      , ("firstPass",  handlers True fields)
      , ("secondPass", handlers False fields)
      , ("allocators", stripStart $ allocators fields)
      ]
    return $ format tmpl ctx

-- | Generate a decoder for a specified type of struct
structDecoder :: Name -> Code
structDecoder name =
  let
    tmpl = pack
       $ "  function _decode__$name(uint p, bytes bs, WireType wt)   \n"
      ++ "      internal constant returns ($typ, uint) {             \n"
      ++ "    var (sz, bytesRead) = _pb_decode_varint(p, bs);        \n"
      ++ "    p += bytesRead;                                        \n"
      ++ "    var (r,) = $lib._decode(p, bs, sz);                    \n"
      ++ "    return (r, sz + bytesRead);                            \n"
      ++ "  }                                                        \n"
    ctx =
      [ ("name", name)
      , ("typ",  qualifiedType name)
      , ("lib",  libraryName name)
      ]
  in
    format tmpl ctx
 
-- | Generate a field reader/setter
fieldReader :: Int -> (Name, FieldType) -> Gen Code
fieldReader i (name, ft) =
  let
    tmpl = pack
       $ "  function $fn(uint p, bytes bs, WireType wt, $msg r, $ctype counts) \n"
      ++ "      internal constant returns (uint) {                             \n"
      ++ "    var (x, sz) = $decoder(p, bs, wt);                               \n"
      ++ "    if(isNil(r)) {                                                   \n" 
      ++ "      $counter += 1;                                                 \n"
      ++ "    } else {                                                         \n"
      ++ "      r.$target = x;                                                 \n"
      ++ "      if($counter > 0) $counter -= 1;                                \n"
      ++ "    }                                                                \n"
      ++ "    return sz;                                                       \n"
      ++ "  }                                                                  \n"
    counter = counterVar i
    target = case fieldLabel ft of
      Repeated -> format "$x[r.$x.length-$c]" [("x", name), ("c", counter)]
      _        -> name
  in do
    ctype  <- counterType
    struct <- getName
    ctx    <- return
      [ ("fn",        readerName name)
      , ("msg",       struct)
      , ("ctype",     ctype)
      , ("decoder",   decoderName ft)
      , ("counter",   counter)
      , ("target",    target)
      ]
    return $ format tmpl ctx

-- | Common protocol buffers implementation library
pbDecoders :: Code
pbDecoders = pack
   $ "  function _pb_decode_uint32(uint p, bytes bs, WireType wt)   \n"
  ++ "      internal constant returns (uint32, uint) {              \n"
  ++ "    if(wt == WireType.Varint) {                               \n"
  ++ "      var (varint, sz) = _pb_decode_varint(p, bs);            \n"
  ++ "      return (uint32(varint), sz);                            \n"
  ++ "    } else if (wt == WireType.Fixed32) {                      \n"
  ++ "      return _pb_decode_uint32f(p, bs);                       \n"
  ++ "    } else throw;                                             \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_uint64(uint p, bytes bs, WireType wt)   \n"
  ++ "      internal constant returns (uint64, uint) {              \n"
  ++ "    if(wt == WireType.Varint) {                               \n"
  ++ "      var (varint, sz) = _pb_decode_varint(p, bs);            \n"
  ++ "      return (uint64(varint), sz);                            \n"
  ++ "    } else if (wt == WireType.Fixed64) {                      \n"
  ++ "      return _pb_decode_uint64f(p, bs);                       \n"
  ++ "    } else throw;                                             \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_int32(uint p, bytes bs, WireType wt)    \n"
  ++ "      internal constant returns (int32, uint) {               \n"
  ++ "    if(wt == WireType.Varint) {                               \n"
  ++ "      var (varint, sz) = _pb_decode_varints(p, bs);           \n"
  ++ "      return (int32(varint), sz);                             \n"
  ++ "    } else if (wt == WireType.Fixed32) {                      \n"
  ++ "      return _pb_decode_int32f(p, bs);                        \n"
  ++ "    } else throw;                                             \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_int64(uint p, bytes bs, WireType wt)    \n"
  ++ "      internal constant returns (int64, uint) {               \n"
  ++ "    if(wt == WireType.Varint) {                               \n"
  ++ "      var (varint, sz) = _pb_decode_varints(p, bs);           \n"
  ++ "      return (int64(varint), sz);                             \n"
  ++ "    } else if (wt == WireType.Fixed64) {                      \n"
  ++ "      return _pb_decode_int64f(p, bs);                        \n"
  ++ "    } else throw;                                             \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_string(uint p, bytes bs, WireType)      \n"
  ++ "      internal constant returns (string, uint) {              \n"
  ++ "    var (x, sz) = _pb_decode_lendelim(p, bs);                 \n"
  ++ "    return (string(x), sz);                                   \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_bytes(uint p, bytes bs, WireType)       \n"
  ++ "      internal constant returns (bytes, uint) {               \n"
  ++ "    return _pb_decode_lendelim(p, bs);                        \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_key(uint p, bytes bs)                   \n"
  ++ "      internal constant returns (uint, WireType, uint) {      \n"
  ++ "    var (x, n) = _pb_decode_varint(p, bs);                    \n"
  ++ "    WireType typeId  = WireType(x & 7);                       \n"
  ++ "    uint fieldId = x / 8; //x >> 3;                           \n"
  ++ "    return (fieldId, typeId, n);                              \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_varint(uint p, bytes bs)                \n"
  ++ "      internal constant returns (uint, uint) {                \n"
  ++ "    uint x = 0;                                               \n"
  ++ "    uint sz = 0;                                              \n"
  ++ "    assembly {                                                \n"
  ++ "      let b := 0                                              \n"
  ++ "      p     := add(bs, p)                                     \n"
  ++ "      loop:                                                   \n"
  ++ "        b  := byte(0, mload(p))                               \n"
  ++ "        x  := or(x, mul(and(0x7f, b), exp(2, mul(7, sz))))    \n"
  ++ "        sz := add(sz, 1)                                      \n"
  ++ "        p  := add(p, 0x01)                                    \n"
  ++ "        jumpi(loop, eq(0x80, and(b, 0x80)))                   \n"
  ++ "    }                                                         \n"
  ++ "    return (x, sz);                                           \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_varints(uint p, bytes bs)               \n"
  ++ "      internal constant returns (int, uint) {                 \n"
  ++ "    var (u, sz) = _pb_decode_varint(p, bs);                   \n"
  ++ "    int s;                                                    \n"
  ++ "    assembly {                                                \n"
  ++ "      s := xor(div(u, 2), add(not(and(u, 1)), 1))             \n"
  ++ "    }                                                         \n"
  ++ "    return (s, sz);                                           \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_uintf(uint p, bytes bs, uint sz)        \n"
  ++ "      internal constant returns (uint, uint) {                \n"
  ++ "    uint x = 0;                                               \n"
  ++ "    assembly {                                                \n"
  ++ "      let i := 0                                              \n"
  ++ "      p     := add(bs, p)                                     \n"
  ++ "      loop:                                                   \n"
  ++ "        jumpi(end, eq(i, sz))                                 \n"
  ++ "        x := or(x, mul(byte(0, mload(p)), exp(2, mul(8, i)))) \n"
  ++ "        p := add(p, 0x01)                                     \n"
  ++ "        i := add(i, 1)                                        \n"
  ++ "        jump(loop)                                            \n"
  ++ "      end:                                                    \n"
  ++ "    }                                                         \n"
  ++ "    return (x, sz);                                           \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_uint32f(uint p, bytes bs)               \n"
  ++ "      internal constant returns (uint32, uint) {              \n"
  ++ "    var (x, sz) = _pb_decode_uintf(p, bs, 4);                 \n"
  ++ "    return (uint32(x), sz);                                   \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_uint64f(uint p, bytes bs)               \n"
  ++ "      internal constant returns (uint64, uint) {              \n"
  ++ "    var (x, sz) = _pb_decode_uintf(p, bs, 8);                 \n"
  ++ "    return (uint64(x), sz);                                   \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_int32f(uint p, bytes bs)                \n"
  ++ "      internal constant returns (int32, uint) {               \n"
  ++ "    var (x, sz) = _pb_decode_uintf(p, bs, 4);                 \n"
  ++ "    int r; assembly { r := x }                                \n"
  ++ "    return (int32(r), sz);                                    \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_int64f(uint p, bytes bs)                \n"
  ++ "      internal constant returns (int64, uint) {               \n"
  ++ "    var (x, sz) = _pb_decode_uintf(p, bs, 8);                 \n"
  ++ "    int r; assembly { r := x }                                \n"
  ++ "    return (int64(r), sz);                                    \n"
  ++ "  }                                                           \n"
  ++ "  function _pb_decode_lendelim(uint p, bytes bs)              \n"
  ++ "      internal constant returns (bytes, uint) {               \n"
  ++ "    var (len, sz) = _pb_decode_varint(p, bs);                 \n"
  ++ "    bytes memory b = new bytes(len);                          \n"
  ++ "    assembly {                                                \n"
  ++ "      let bptr  := add(b, 32)                                 \n"
  ++ "      let count := 0                                          \n"
  ++ "      p         := add(add(bs, p),sz)                         \n"
  ++ "      loop :                                                  \n"
  ++ "        jumpi(end, eq(count, len))                            \n"
  ++ "        mstore8(bptr, byte(0, mload(p)))                      \n"
  ++ "        p     := add(p, 1)                                    \n"
  ++ "        bptr  := add(bptr, 1)                                 \n"
  ++ "        count := add(count, 1)                                \n"
  ++ "        jump(loop)                                            \n"
  ++ "      end:                                                    \n"
  ++ "    }                                                         \n"
  ++ "    return (b, sz+len);                                       \n"
  ++ "  }                                                           \n"

