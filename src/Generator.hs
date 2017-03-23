{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Generator where

import Data.Map (Map)
import Data.Text (pack, strip)
import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Monad.Result
import Types
import Util
import qualified Library
import qualified Build


generate :: [Struct] -> App Code
generate structs =
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
      ++  "$frontendLibrary                                                        \n"
      ++  "$codecLibraries                                                         \n"
      ++  "$runtimeLibrary                                                         \n"
      ++  "                                                                        \n"
  in do
    codecLibraries <- T.concat <$> mapM Library.generate structs
    
    return . stripLineEndings $ format tmpl
      [ ("solidityVersion",  "^0.4.0")
      , ("codecLibraries",   strip codecLibraries)
      , ("runtimeLibrary",   $(Build.pbLibrary))
      , ("frontendLibrary",  strip $ generateFrontend "Types" structs)
      ]

generateFrontend :: Name -> [Struct] -> Code
generateFrontend name structs =
  let
    tmpl = pack
       $ "library $name {               \n"
      ++ "  $structDefs                 \n"
      ++ "  $delegators                 \n"
      ++ "}                             \n"

    makeDelegators :: Struct -> Code
    makeDelegators (Struct name fields) = 
      let
        tmpl = pack
           $ "  function decode$name(bytes bs) internal constant returns($name) {\n"
          ++ "    $name memory r;                                                \n"
          ++ "    var x = $lib.decode(bs);                                       \n"
          ++ "    assembly { r := x }                                            \n"
          ++ "    return r;                                                      \n"
          ++ "  }                                                                \n"
          ++ "  function encode$name($name x) internal constant returns(bytes) { \n"
          ++ "    $lib.$name memory xx;                                          \n"
          ++ "    assembly { xx := x }                                           \n"
          ++ "    return $lib.encode(xx);                                        \n"
          ++ "  }                                                                \n"
          ++ "  function store$name($name memory input, $name storage output)    \n"
          ++ "      internal constant {                                          \n"
          ++ "    $lib.$name memory input2;                                      \n"
          ++ "    $lib.$name storage output2;                                    \n"
          ++ "    assembly {                                                     \n"
          ++ "      input2 := input                                              \n"
          ++ "      output2 := output                                            \n"
          ++ "    }                                                              \n"
          ++ "    return $lib.store(input2, output2);                            \n"
          ++ "  }                                                                \n"
      in
        format tmpl [("name", name), ("lib", libraryName name)]
    
    structDefs = T.concat $ map (Library.structDefinition False) structs
  in
    format tmpl
      [ ("name",       name)
      , ("structDefs", strip structDefs)
      , ("delegators", strip . T.concat $ map makeDelegators structs)
      ]

