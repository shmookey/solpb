{-# LANGUAGE OverloadedStrings #-}

module Util.TestGen where

import Prelude hiding (fail)
import Data.Text (Text, pack, unpack, strip)
import Data.Char (ord)
import Data.Semigroup ((<>))
import Data.ByteString.Lazy.Char8 (ByteString)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T

import qualified Text.DescriptorProtos.FileDescriptorProto as FDP

import Util.ReSpec
import Control.Monad.Resultant
import Util (format)
import Util.Protobuf (getStruct)
import Types (Name, Code, Struct(Struct))
import qualified Convert


data Value
  = Integer Integer
  | Bool Bool
  | String String
  | Bytes ByteString 

fieldCheck :: Name -> Value -> Code
fieldCheck name value = pack $ case value of
  Integer x  -> printf "    if(x.%s != %i) throw;    \n" name x
  Bool False -> printf "    if(x.%s != false) throw; \n" name
  Bool True  -> printf "    if(x.%s != true) throw;  \n" name
  String xs  -> 
    let 
      byte :: Int -> Int -> String
      byte i b = printf "    if(bytes(x.%s)[%i] != %i) throw;  \n" name i b
    in
      mconcat . map (uncurry byte) . zip [0..] $ map ord xs
  Bytes bs   -> 
    let 
      byte :: Int -> Int -> String
      byte i b = printf "    if(x.%s[%i] != %i) throw;  \n" name i b
    in
      mconcat . map (uncurry byte) . zip [0..] . map ord $ BL8.unpack bs

generateTestContract :: Struct -> [(Name, Value)] -> Spec Code
generateTestContract (Struct name fields) checks =
  let
    tmpl = pack
       $ "contract ${name}Spec {                             \n"
      ++ "  function testDecode(bytes bs) returns (bool) {   \n"
      ++ "    uint i;                                        \n"
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

    fieldSetters = pack ""
 
  in return $ format tmpl
    [ ("name",         name)
    , ("lib",          name <> "Codec")
    , ("fieldChecks",  strip . mconcat $ map (uncurry fieldCheck) checks)
    , ("fieldSetters", fieldSetters)
    ]

onFailure :: Code -> Code -> String -> String -> String -> Spec Text
onFailure code callData objData testName err = do
  lift . putStrLn $ "Failed: "                 ++ testName
  lift . putStrLn $ "The error was: "          ++ err
  lift . putStrLn $ "Call data: "              ++ (unpack callData)
  lift . putStrLn $ "Object data: "            ++ objData
  lift . putStrLn $ "Test contract source: \n" ++ (unpack code)
  return "Error"
  
