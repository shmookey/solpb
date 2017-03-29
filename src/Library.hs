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
createLibrary struct =
  let
    tmpl = pack
       $  "library ${struct}Codec {      \n"
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
      [ ("struct",           structName struct)
      , ("structDefinition", strip $ structDefinition True struct)
      , ("decoderSection",   strip $ decoderSection)
      , ("encoderSection",   strip $ encoderSection)
      , ("utilityFunctions", strip $ utilityFunctions struct)
      , ("storeFunction",    strip $ storeFunction struct)
      ]

structDefinition :: Bool -> Struct -> Code
structDefinition qualifiedNames struct =
  let
    tmpl = pack
       $ "  struct $name {    \n"
     ++  "    $fields         \n"
     ++  "  }                 \n"

    formatField :: Field -> Code
    formatField fld = 
      let
        tmpl = pack "    $type $name; "
      in
        format tmpl
          [ ("name", fieldName fld)
          , ("type", formatFieldType Normal fld)
          ]

    fields = T.unlines . map formatField $ structFields struct
  in
    format tmpl
      [ ("name",   structName struct)
      , ("fields", strip fields)
      ]

utilityFunctions :: Struct -> Code
utilityFunctions struct =
  let
    tmpl = pack
       $ "  function nil() internal constant returns ($name r) {         \n"
      ++ "    assembly { r := 0 }                                        \n"
      ++ "  }                                                            \n"
      ++ "  function isNil($name x) internal constant returns (bool r) { \n"
      ++ "    assembly { r := iszero(x) }                                \n"
      ++ "  }                                                            \n"
  in
    format tmpl [("name", structName struct)]

storeFunction :: Struct -> Code
storeFunction struct =
  let
    tmpl = pack
       $ "  function store($x memory input, $x storage output) internal { \n"
      ++ "    $fieldStores                                                \n"
      ++ "  }                                                             \n"

    storeField :: Field -> Code
    storeField fld =
      let
        tmpl = pack $
          if
            isStruct fld && isRepeated fld
          then
               "    output.$field.length = input.$field.length;          \n"
            ++ "    for(uint i$i=0; i$i<input.$field.length; i$i++)      \n"
            ++ "      $lib.store(input.$field[i$i], output.$field[i$i]); \n"
          else if
            isStruct fld
          then
              "    $lib.store(input.$field, output.$field);              \n"
          else
              "    output.$field = input.$field;                         \n" 
      in
        format tmpl
          [ ("field", fieldName fld)
          , ("lib",   formatLibrary $ fieldType fld)
          , ("i",     show' $ fieldID fld)
          ]

  in
    format tmpl
      [ ("x",            structName struct)
      , ("fieldStores",  strip . mconcat . map storeField $ structFields struct)
      ]

