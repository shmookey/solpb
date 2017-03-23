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
      ++  "$codecLibraries                                                         \n"
      ++  "$runtimeLibrary                                                         \n"
      ++  "                                                                        \n"
  in do
    codecLibraries <- T.concat <$> mapM Library.generate structs
    
    return $ format tmpl
      [ ("solidityVersion",  "^0.4.0")
      , ("codecLibraries",   strip codecLibraries)
      , ("runtimeLibrary",   $(Build.pbLibrary))
      ]


