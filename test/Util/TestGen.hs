{-# LANGUAGE OverloadedStrings #-}

module Util.TestGen where

import Prelude hiding (fail)
import Data.Text (Text, pack, unpack, strip)
import qualified Data.Text as T

import qualified Text.DescriptorProtos.FileDescriptorProto as FDP

import Control.Monad.Resultant
import Util (format)
import Util.Protobuf (getStruct)
import Types
import qualified Convert


generateTestContract :: Struct -> [(Name, Code)] -> App Code
generateTestContract (Struct name fields) kvs =
  let
    tmpl = pack
       $ "contract ${name}Spec {                             \n"
      ++ "  function testDecode(bytes bs) returns (bool) {   \n"
      ++ "    $lib.$name memory x = $lib.decode(bs);         \n"
      ++ "    $fieldChecks                                   \n"
      ++ "    return true;                                   \n"
      ++ "  }                                                \n"
      ++ "  function testEncode() returns (bytes) {          \n"
      ++ "    $lib.$name memory x;                           \n"
      ++ "    $fieldSetters                                  \n"
      ++ "    bytes memory bs = $lib.encode(x);              \n"
      ++ "    return bs;                                     \n"
      ++ "  }                                                \n"
      ++ "}                                                  \n"

    mkCheck :: Name -> Code -> Code
    mkCheck key val =
      let
        tmpl = pack 
           $ "    if(x.$key != $val) throw; \n"
      in
        format tmpl
          [ ("key", key)
          , ("val", val)
          ]

    fieldSetters = pack ""
    fieldChecks = T.concat $ map (uncurry mkCheck) kvs
 
  in return $ format tmpl
    [ ("name",         name)
    , ("lib",          libraryName name)
    , ("fieldChecks",  strip fieldChecks)
    , ("fieldSetters", fieldSetters)
    ]


onFailure :: Code -> Code -> String -> String -> String -> App Text
onFailure code callData objData testName err = do
  lift . putStrLn $ "Failed: "                 ++ testName
  lift . putStrLn $ "The error was: "          ++ err
  lift . putStrLn $ "Call data: "              ++ (unpack callData)
  lift . putStrLn $ "Object data: "            ++ objData
  lift . putStrLn $ "Test contract source: \n" ++ (unpack code)
  return "Error"
  
