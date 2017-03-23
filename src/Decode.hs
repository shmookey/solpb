{-# LANGUAGE OverloadedStrings #-}

module Decode where

import qualified Data.Text.Template as Template
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text, pack, strip, stripStart)
import Data.List (nub)

import Types
import Util

-- | Name of the decoder function for a given field's scalar type
decoderName :: FieldType -> Name
decoderName ft = T.concat $ case ft of
  User x _ -> ["_decode__", x]
  Sol x _  -> ["_pb._decode_sol_", fieldTypeName ft]
  _        -> ["_pb._decode_", fieldTypeName ft]

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
counterType :: Int -> Name
counterType n = T.concat ["uint[", pack $ show n, "] memory"]

generate :: Struct -> Code
generate struct@(Struct name fields) = 
  let
    tmpl = pack
       $ "  // Decoder section                           \n"
      ++ "                                               \n"
      ++ "  $mainDecoder                                 \n"
      ++ "  $innerDecoder                                \n"
      ++ "  $fieldReaders                                \n"
      ++ "  $structDecoders                              \n"

    n              = Map.size fields
    mainDecoder    = generateMainDecoder name
    innerDecoder   = generateInnerDecoder struct
    fieldReaders   = T.concat . map (uncurry $ generateFieldReader name n) 
                     $ Map.toList fields
    structDecoders = T.concat . map generateStructDecoder . nub
                     . map fieldTypeName . snd . unzip . Map.elems
                     $ Map.filter (isStruct . snd) fields
  in
    format tmpl
      [ ("mainDecoder",    stripStart mainDecoder)
      , ("innerDecoder",   stripStart innerDecoder)
      , ("fieldReaders",   stripStart fieldReaders)
      , ("structDecoders", stripStart structDecoders)
      ]

generateMainDecoder :: Name -> Code
generateMainDecoder name =
  let
    tmpl = pack
       $ "  function decode(bytes bs) internal constant returns ($name) { \n"
      ++ "    var (x,) = _decode(32, bs, bs.length);                      \n"
      ++ "    return x;                                                   \n"
      ++ "  }                                                             \n"
  in
    format tmpl [("name", name)]

generateInnerDecoder :: Struct -> Code
generateInnerDecoder (Struct name fields) =
  let
    tmpl = pack
       $ "  function _decode(uint p, bytes bs, uint sz)                 \n"
      ++ "      internal constant returns ($name, uint) {               \n"
      ++ "    $name memory r;                                           \n"
      ++ "    $ctype counts;                                            \n"
      ++ "    uint fieldId;                                             \n"
      ++ "    _pb.WireType wireType;                                    \n"
      ++ "    uint bytesRead;                                           \n"
      ++ "    uint offset = p;                                          \n"
      ++ "    while(p < offset+sz) {                                    \n"
      ++ "      (fieldId, wireType, bytesRead) = _pb._decode_key(p, bs);\n"
      ++ "      p += bytesRead;                                         \n"
      ++ "      $firstPass                                              \n"
      ++ "    }                                                         \n"
      ++ "    p = offset;                                               \n"
      ++ "    $allocators                                               \n"
      ++ "    while(p < offset+sz) {                                    \n"
      ++ "      (fieldId, wireType, bytesRead) = _pb._decode_key(p, bs);\n"
      ++ "      p += bytesRead;                                         \n"
      ++ "      $secondPass                                             \n"
      ++ "    }                                                         \n"
      ++ "    return (r, sz);                                           \n"
      ++ "  }                                                           \n"

    -- | `fieldHandler i ft firstPass` handles a field by ID, type and pass
    fieldHandler :: Bool -> Int -> Field -> Code
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
    arrayAllocator :: Int -> Field -> Code
    arrayAllocator i (name, ft) = format line ctx
      where line = "    r.$name = new $typ($counter);"
            ctx  = [("name", name), ("typ", typ), ("counter", counterVar i)]
            typ  = fieldType ft

    -- | `handlers True` generates field handlers for the first pass
    handlers :: Bool -> Map Int Field -> Code
    handlers firstPass = 
        flip T.append "      else throw;"                  -- insert closing 'else'
      . T.drop 5 . T.stripStart                            -- delete leading 'else'
      . T.concat . map (uncurry $ fieldHandler firstPass)
      . Map.toList

    -- | Generate array allocators
    allocators :: Map Int Field -> Code
    allocators = T.intercalate "\n"
      . map (uncurry arrayAllocator)
      . Map.toList
      . Map.filter (isRepeated . snd)

  in
    format tmpl
      [ ("name",       name)
      , ("ctype",      counterType $ Map.size fields)
      , ("firstPass",  handlers True fields)
      , ("secondPass", handlers False fields)
      , ("allocators", stripStart $ allocators fields)
      ]

-- | Generate a decoder for a specified type of struct
generateStructDecoder :: Name -> Code
generateStructDecoder name =
  let
    tmpl = pack
       $ "  function _decode__$name(uint p, bytes bs, _pb.WireType wt)\n"
      ++ "      internal constant returns ($typ, uint) {              \n"
      ++ "    var (sz, bytesRead) = _pb._decode_varint(p, bs);        \n"
      ++ "    p += bytesRead;                                         \n"
      ++ "    var (r,) = $lib._decode(p, bs, sz);                     \n"
      ++ "    return (r, sz + bytesRead);                             \n"
      ++ "  }                                                         \n"
    ctx =
      [ ("name", name)
      , ("typ",  qualifiedType name)
      , ("lib",  libraryName name)
      ]
  in
    format tmpl ctx
 
-- | Generate a field reader/setter. `n` denotes total number of fields
generateFieldReader :: Name -> Int -> Int -> Field -> Code
generateFieldReader struct n i (name, ft) =
  let
    tmpl = pack
       $ "  function $fn(uint p, bytes bs, _pb.WireType wt, $msg r, $ctype counts) \n"
      ++ "      internal constant returns (uint) {                                 \n"
      ++ "    var (x, sz) = $decoder(p, bs, wt);                                   \n"
      ++ "    if(isNil(r)) {                                                       \n" 
      ++ "      $counter += 1;                                                     \n"
      ++ "    } else {                                                             \n"
      ++ "      r.$target = x;                                                     \n"
      ++ "      if($counter > 0) $counter -= 1;                                    \n"
      ++ "    }                                                                    \n"
      ++ "    return sz;                                                           \n"
      ++ "  }                                                                      \n"
    counter = counterVar i
    target = case fieldLabel ft of
      Repeated -> format "$x[r.$x.length-$c]" [("x", name), ("c", counter)]
      _        -> name
  in
    format tmpl   
      [ ("fn",        readerName name)
      , ("msg",       struct)
      , ("ctype",     counterType n)
      , ("decoder",   decoderName ft)
      , ("counter",   counter)
      , ("target",    target)
      ]

