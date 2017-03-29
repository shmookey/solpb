{-# LANGUAGE OverloadedStrings #-}

module Decode where

import Data.Map (Map)
import Data.Text (Text, pack, strip, stripStart)
import Data.List (nub)
import Data.Semigroup ((<>))
import qualified Data.Text.Template as Template
import qualified Data.Text as T
import qualified Data.Map as Map

import Types
import Util


-- | Name of the decoder function for a given field's scalar type
decoderName :: Field -> Name
decoderName fld =
  let
    name = formatFieldType Quote fld
  in
    case fieldType fld of
      LenDelim (Message (User name lib)) -> "_decode__"        <> name
      LenDelim (Message (Sol name))      -> "_pb._decode_sol_" <> name
      _                                  -> "_pb._decode_"     <> name

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

    n              = length fields
    mainDecoder    = generateMainDecoder name
    innerDecoder   = generateInnerDecoder struct
    fieldReaders   = mconcat $ map (generateFieldReader struct) fields
    structDecoders = generateStructDecoders fields
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
       $ "  function _decode(uint p, bytes bs, uint sz)                   \n"
      ++ "      internal constant returns ($name, uint) {                 \n"
      ++ "    $name memory r;                                             \n"
      ++ "    uint[$n] memory counters;                                   \n"
      ++ "    uint fieldId;                                               \n"
      ++ "    _pb.WireType wireType;                                      \n"
      ++ "    uint bytesRead;                                             \n"
      ++ "    uint offset = p;                                            \n"
      ++ "    while(p < offset+sz) {                                      \n"
      ++ "      (fieldId, wireType, bytesRead) = _pb._decode_key(p, bs);  \n"
      ++ "      p += bytesRead;                                           \n"
      ++ "      $firstPass                                                \n"
      ++ "    }                                                           \n"
      ++ "    p = offset;                                                 \n"
      ++ "    $allocators                                                 \n"
      ++ "    while(p < offset+sz) {                                      \n"
      ++ "      (fieldId, wireType, bytesRead) = _pb._decode_key(p, bs);  \n"
      ++ "      p += bytesRead;                                           \n"
      ++ "      $secondPass                                               \n"
      ++ "    }                                                           \n"
      ++ "    return (r, sz);                                             \n"
      ++ "  }                                                             \n"

    -- | `fieldHandler i ft firstPass` handles a field by ID, type and pass
    fieldHandler :: Bool -> Field -> Code
    fieldHandler firstPass fld =
      let
        tmpl = pack
           $ "      else if(fieldId == $id)         \n"
          ++ "        p += _read_$k($args);         \n"

        args = case (fieldLabel fld, firstPass) of
          (Repeated, True)  -> "p, bs, wireType, nil(), counters"
          (Repeated, False) -> "p, bs, wireType, r, counters"
          (_       , True)  -> "p, bs, wireType, r, counters"
          (_       , False) -> "p, bs, wireType, nil(), counters"

        ctx =
          [ ("id",     show' $ fieldID fld)
          , ("k",      show' $ fieldName fld)
          , ("args",   args)
          ]
      in
        format tmpl ctx

    -- | Make code to initialise an array field's length from its counter
    arrayAllocator :: Field -> Code
    arrayAllocator fld = 
      let
        tmpl = "    r.$name = new $typ(counters[$i]);"
      in
       format tmpl
         [ ("name", name)
         , ("typ",  formatFieldType Normal fld)
         , ("i",    show' $ fieldID fld)
         ]

    -- | `handlers True` generates field handlers for the first pass
    handlers :: Bool -> [Field] -> Code
    handlers firstPass = 
        flip T.append "      else throw;"                  -- insert closing 'else'
      . T.drop 5 . T.stripStart                            -- delete leading 'else'
      . mconcat . map (fieldHandler firstPass)

    -- | Generate array allocators
    allocators :: [Field] -> Code
    allocators = T.intercalate "\n"
      . map arrayAllocator
      . filter isRepeated

  in
    format tmpl
      [ ("name",       name)
      , ("n",          show' $ length fields)
      , ("firstPass",  handlers True fields)
      , ("secondPass", handlers False fields)
      , ("allocators", stripStart $ allocators fields)
      ]

-- | Generate a decoder for a specified type of struct
generateStructDecoders :: [Field] -> Code
generateStructDecoders =
  let
    structDecoder :: ValueType -> Code
    structDecoder typ =
      let
        tmpl = pack
           $ "  function _decode__$name(uint p, bytes bs, _pb.WireType wt) \n"
          ++ "      internal constant returns ($typ, uint) {               \n"
          ++ "    var (sz, bytesRead) = _pb._decode_varint(p, bs);         \n"
          ++ "    p += bytesRead;                                          \n"
          ++ "    var (r,) = $lib._decode(p, bs, sz);                      \n"
          ++ "    return (r, sz + bytesRead);                              \n"
          ++ "  }                                                          \n"
      in
        format tmpl
          [ ("name", formatValueType Quote typ)
          , ("typ",  formatValueType Scalar typ)
          , ("lib",  formatLibrary typ)
          ]
  in    
    mconcat . map structDecoder 
            . nub
            . map fieldType
            . filter isStruct
 
-- | Generate a field reader/setter. `n` denotes total number of fields
generateFieldReader :: Struct -> Field -> Code
generateFieldReader (Struct name fields) fld =
  let
    tmpl = pack
       $ "  function _read_$k(uint p, bytes bs, _pb.WireType wt, $msg r, uint[$n] counters) \n"
      ++ "      internal constant returns (uint) {                                          \n"
      ++ "    var (x, sz) = $decoder(p, bs, wt);                                            \n"
      ++ "    if(isNil(r)) {                                                                \n" 
      ++ "      counters[$i] += 1;                                                          \n"
      ++ "    } else {                                                                      \n"
      ++ "      $target = x;                                                                \n"
      ++ "      if(counters[$i] > 0) counters[$i] -= 1;                                     \n"
      ++ "    }                                                                             \n"
      ++ "    return sz;                                                                    \n"
      ++ "  }                                                                               \n"
    target = case fieldLabel fld of
      Repeated -> format "r.$k[ r.$k.length - counters[$i] ]" 
                     [ ("k", fieldName fld)
                     , ("i", show' $ fieldID fld) ]
      _        -> "r." <> fieldName fld
  in
    format tmpl   
      [ ("k",       fieldName fld)
      , ("i",       show' $ fieldID fld)
      , ("n",       show' $ length fields)
      , ("msg",     name)
      , ("decoder", decoderName fld)
      , ("target",  target)
      ]

