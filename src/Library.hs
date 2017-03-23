{-# LANGUAGE OverloadedStrings #-}

module Library where

import Data.Map (Map)
import Data.Text (pack, strip, stripStart)
import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Monad.Resultant
import Control.Monad.Result

import qualified Decode
import qualified Encode
import Types
import Util


generate :: Struct -> App Code
generate struct = return $ createLibrary struct

createLibrary :: Struct -> Code
createLibrary struct@(Struct name fields) =
  let
    tmpl = pack
       $  "library $name {               \n"
      ++  "  $structDefinition           \n"
      ++  "  $decoderSection             \n"
      ++  "  $encoderSection             \n"
      ++  "  $storeFunction              \n"
      ++  "  $utilityFunctions           \n"
      ++  "}                             \n"

    decoderSection = Decode.generate struct
    encoderSection = Encode.generate struct

  in    
    format tmpl
      [ ("name",             libraryName name)
      , ("structDefinition", strip $ structDefinition True struct)
      , ("decoderSection",   strip $ decoderSection)
      , ("encoderSection",   strip $ encoderSection)
      , ("utilityFunctions", strip $ utilityFunctions name)
      , ("storeFunction",    strip $ storeFunction struct)
      ]

structDefinition :: Bool -> Struct -> Code
structDefinition qualifiedNames (Struct name kvs) =
  let
    tmpl = pack
       $ "  struct $name {    \n"
     ++  "    $fields         \n"
     ++  "  }                 \n"

    formatField :: Field -> Code
    formatField (x, ft) = format "    $type $name; \n" $
      if qualifiedNames then [("name", x), ("type", fieldType ft)]
                        else [("name", x), ("type", fieldTypeName ft)]

    fields = Map.foldl (\a -> T.append a . formatField) "" kvs
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

storeFunction :: Struct -> Code
storeFunction (Struct name kvs) =
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

