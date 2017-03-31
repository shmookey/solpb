{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Generator where

import Data.Map (Map)
import Data.Text (pack, strip)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Monad.Result
import Types
import Util
import qualified Library
import qualified Build


banner = pack
  $  "                                                                        \n"
 ++  "//----------------------------------------------------------------------\n"
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
 ++  "//----------------------------------------------------------------------\n"
 ++  "                                                                        \n"

generate :: [Struct] -> App Code
generate structs =
  let
    tmpl = pack
       $  "$versionPragma                                                          \n"
      ++  "$banner                                                                 \n"
      ++  "$packageLibrary                                                         \n"
      ++  "$codecLibraries                                                         \n"
      ++  "$runtimeLibrary                                                         \n"
      ++  "                                                                        \n"
  in do
    codecLibraries <- T.concat <$> mapM Library.generate structs
    noPragma       <- optNoPragma <$> getConfig
    libName        <- pack . optLibName <$> getConfig  
    separate       <- optSeparate <$> getConfig
    noRuntime      <- optNoRuntime <$> getConfig
    noCombine      <- optNoCombine <$> getConfig
    versionPragma  <- generateVersionPragma

    let runtimeLibrary = if noRuntime || separate 
                         then ""
                         else generateRuntimeLibrary

    packageLibrary    <- if noCombine
                         then return ""
                         else generatePackageLibrary libName structs
 
    return . stripLineEndings $ format tmpl
      [ ("versionPragma",    strip versionPragma)
      , ("banner",           banner)
      , ("codecLibraries",   strip codecLibraries)
      , ("runtimeLibrary",   strip runtimeLibrary)
      , ("packageLibrary",   strip packageLibrary)
      ]

generateRuntimeLibrary :: Code
generateRuntimeLibrary = $(Build.pbLibrary)

generateVersionPragma :: App Code
generateVersionPragma = (optNoPragma <$> getConfig) >>= \noPragma -> return $
  if noPragma then pack "" else "pragma solidity ^0.4.0;"

generatePackageLibrary :: Name -> [Struct] -> App Code
generatePackageLibrary name structs = getConfig >>= \config ->
  let
    tmpl = pack
       $ "$versionPragma                \n"
      ++ "$banner                       \n"
      ++ "library $name {               \n"
      ++ "  $structDefs                 \n"
      ++ "  $delegators                 \n"
      ++ "}                             \n"

    makeDelegators :: Struct -> Code
    makeDelegators struct = 
      let
        tmpl = pack
           $ "  function decode$struct(bytes bs) internal constant returns($struct) { \n"
          ++ "    $struct memory r;                                                   \n"
          ++ "    var x = $lib.decode(bs);                                            \n"
          ++ "    assembly { r := x }                                                 \n"
          ++ "    return r;                                                           \n"
          ++ "  }                                                                     \n"
          ++ "  function encode$struct($struct x) internal constant returns(bytes) {  \n"
          ++ "    $lib.$struct memory xx;                                             \n"
          ++ "    assembly { xx := x }                                                \n"
          ++ "    return $lib.encode(xx);                                             \n"
          ++ "  }                                                                     \n"
          ++ "  function store$struct($struct memory input, $struct storage output)   \n"
          ++ "      internal constant {                                               \n"
          ++ "    $lib.$struct memory input2;                                         \n"
          ++ "    $lib.$struct storage output2;                                       \n"
          ++ "    assembly {                                                          \n"
          ++ "      input2 := input                                                   \n"
          ++ "      output2 := output                                                 \n"
          ++ "    }                                                                   \n"
          ++ "    return $lib.store(input2, output2);                                 \n"
          ++ "  }                                                                     \n"
      in
        format tmpl
          [ ("struct", structName struct)
          , ("lib",    structName struct <> "Codec")
          ]
    
    structDefs = T.concat $ map (Library.structDefinition False) structs
    bannerText = if optSeparate config then banner else ""
  in do
    pragma <- generateVersionPragma
    return $ format tmpl
      [ ("name",          name)
      , ("banner",        bannerText)
      , ("versionPragma", if optSeparate config then pragma else "")
      , ("structDefs",    strip structDefs)
      , ("delegators",    strip . mconcat $ map makeDelegators structs)
      ]

