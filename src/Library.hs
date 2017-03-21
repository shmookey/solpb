{-# LANGUAGE OverloadedStrings #-}

module Library where

import Data.Map (Map)
import Data.Text (pack, strip, stripStart)
import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Monad.Resultant
import Control.Monad.Result

import qualified Decode
import Gen
import Types
import Util


run :: Name -> Map Int (Name, FieldType) -> Result Error Code
run x kvs = snd . runResultant createLibrary $ State mempty x kvs

createLibrary :: Gen Code
createLibrary =
  let
    tmpl = pack
       $  "pragma solidity $solidityVersion;                                       \n"
      ++  "                                                                        \n"
      ++  "// AUTO-GENERATED FILE - DO NOT EDIT                                    \n"
      ++  "//                                                                      \n"
      ++  "// This file was generated automatically by solpb from data structures  \n"
      ++  "// defined elsewhere. Instead of editing this file, consider modifying  \n"
      ++  "// the source schema or writing a separate library.                     \n" 
      ++  "//                                                                      \n"
      ++  "// solpb is a free and open-source implementation of protocol buffers   \n"
      ++  "// for Solidity. Protocol buffers are an efficient serialisation format \n"
      ++  "// for structured data developed by Google. More information about this \n"
      ++  "// project is available from the repository page:                       \n"
      ++  "//                                                                      \n"
      ++  "//   http://github.com/CBAInnovationLab/protobuf-solidity               \n"
      ++  "//                                                                      \n"
      ++  "// Further information about protocol buffers, including tutorials and  \n"
      ++  "// downloads for implementations in most major languages is available at\n"
      ++  "// the protocol buffers website:                                        \n"
      ++  "//                                                                      \n"
      ++  "//   https://developers.google.com/protocol-buffers/                    \n"
      ++  "//                                                                      \n"
      ++  "//  Development of solpb is proudly sponsored by the Innovation Lab at  \n"
      ++  "//  the Commonwealth Bank of Australia.                                 \n"
      ++  "                                                                        \n"
      ++  "library $libraryName {                                                  \n"
      ++  "                                                                        \n"
      ++  "  $structDefinition                                                     \n"
      ++  "  $decoderSection                                                       \n"
      ++  "  $storeFunction                                                        \n"
      ++  "  $utilityFunctions                                                     \n"
      ++  "}                                                                       \n"
  in do
    name    <- getName
    fields  <- getFields
    decoder <- stripStart <$> Decode.generate
    
    return $ format tmpl
      [ ("solidityVersion",  "^0.4.0")
      , ("libraryName",      libraryName name)
      , ("structDefinition", stripStart $ structDefinition name fields)
      , ("decoderSection",   decoder)
      , ("utilityFunctions", stripStart $ utilityFunctions name)
      , ("storeFunction",    stripStart $ storeFunction name fields)
      ]

structDefinition :: Name -> Map Int (Name, FieldType) -> Code
structDefinition name kvs =
  let
    tmpl = pack
       $ "  struct $name {    \n"
     ++  "    $fields         \n"
     ++  "  }                 \n"

    formatField :: (Name, FieldType) -> Code
    formatField (x, ft) = format "    $type $name;\n" ctx
      where ctx = [("name", x), ("type", fieldType ft)]

    fields = T.concat . map formatField $ Map.elems kvs
  in
    format tmpl
      [ ("name",   name)
      , ("fields", strip fields)
      ]

utilityFunctions :: Name -> Code
utilityFunctions name =
  let
    tmpl = pack
       $ "  enum WireType { Varint, Fixed64, LengthDelim, Fixed32 }      \n"
      ++ "  function nil() internal constant returns ($name r) {         \n"
      ++ "    assembly { r := 0 }                                        \n"
      ++ "  }                                                            \n"
      ++ "  function isNil($name x) internal constant returns (bool r) { \n"
      ++ "    assembly { r := iszero(x) }                                \n"
      ++ "  }                                                            \n"
  in
    format tmpl [("name", name)]

storeFunction :: Name -> Map Int (Name, FieldType) -> Code
storeFunction name kvs =
  let
    tmpl = pack
       $ "  function store($x memory input, $x storage output) internal { \n"
      ++ "    $builtins                                                   \n"
      ++ "    $structScalars                                              \n"
      ++ "    $structArrays                                               \n"
      ++ "  }                                                             \n"

    builtin :: Name -> Code
    builtin x  = format "    output.$x = input.$x; \n" [("x",x)]

    structScalar :: Name -> Name -> Code
    structScalar lib x = format "    $lib.store(input.$x, output.$x); \n"
      [("lib", lib), ("x", x)]

    structArray :: Name -> Name -> Code
    structArray lib x =
      let
        tmpl = pack
           $ "    output.$x.length = input.$x.length;         \n"
          ++ "    for(uint $i=0; $i<input.$x.length; $i++)    \n"
          ++ "      $lib.store(input.$x[$i], output.$x[$i]);  \n"
        index = T.append x "_index"
      in
        format tmpl [("lib",lib),("x",x),("i",index)]

    builtins = T.concat
     . map (builtin . fst) 
     . Map.elems
     $ Map.filter (not . isStruct . snd) kvs

    structScalars = T.concat
       . map (\(x,ft) -> structScalar (fieldLibrary ft) x)
       . Map.elems
       $ Map.filter (\(_,ft) -> isStruct ft && not (isRepeated ft)) kvs
    
    structArrays = T.concat
       . map (\(x,ft) -> structArray (fieldLibrary ft) x)
       . Map.elems
       $ Map.filter (\(_,ft) -> isStruct ft && isRepeated ft) kvs
  in
    format tmpl
      [ ("x",             name)
      , ("builtins",      strip builtins)
      , ("structScalars", strip structScalars)
      , ("structArrays",  strip structArrays)
      ]

