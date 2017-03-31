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
  | IntList String [Integer]
  | BytesList [ByteString]
  | SolType String String

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


    fieldCheck :: Name -> Value -> Code
    fieldCheck name value = pack $ case value of
      Integer x       -> printf "    if(x.%s != %i) throw;    \n" name x
      Bool False      -> printf "    if(x.%s != false) throw; \n" name
      Bool True       -> printf "    if(x.%s != true) throw;  \n" name
      SolType typ val -> printf "    if(x.%s != %s) throw;    \n" name val
      String xs  -> 
        let 
          byte :: Int -> Int -> String
          byte i b = printf "    if(bytes(x.%s)[%i] != %i) throw;  \n" name i b
        in
          mconcat . map (uncurry byte) . zip [0..] $ map ord xs
      Bytes bs -> 
        let 
          byte :: Int -> Int -> String
          byte i b = printf "    if(x.%s[%i] != %i) throw;  \n" name i b
        in
          mconcat . map (uncurry byte) . zip [0..] . map ord $ BL8.unpack bs
      IntList _ xs ->
        let
          elem :: Int -> Integer -> String
          elem i v = printf "    if(x.%s[%i] != %i) throw;  \n" name i v
        in
          mconcat . map (uncurry elem) $ zip [0..] xs
      BytesList bss -> 
        let 
          bytes :: Int -> ByteString -> String
          bytes i bs =
            let 
              byte :: Int -> Int -> String
              byte j b = printf "    if(x.%s[%i][%i] != %i) throw;  \n" name i j b
            in
              mconcat . map (uncurry byte) . zip [0..] . map ord $ BL8.unpack bs
        in
          mconcat . map (uncurry bytes) $ zip [0..] bss

    fieldSet :: Name -> Value -> Code
    fieldSet name value = pack $ case value of
      Integer x       -> printf "    x.%s = %i;    \n" name x
      Bool False      -> printf "    x.%s = false; \n" name
      Bool True       -> printf "    x.%s = true;  \n" name
      SolType typ val -> printf "    x.%s = %s;    \n" name val
      String xs  -> 
        let 
          byte :: Int -> Int -> String
          byte i b = printf "    b_%s[%i] = %i; \n" name i b

          alloc = printf "    bytes memory b_%s = new bytes(%i); \n" name (length xs)
          cast  = printf "    x.%s = string(b_%s);               \n" name name
          bytes = mconcat . map (uncurry byte) . zip [0..] $ map ord xs
        in
          mconcat [alloc, bytes, cast]
      Bytes bs -> 
        let
          byte :: Int -> Int -> String
          byte i b = printf "    x.%s[%i] = %i; \n" name i b

          alloc = printf "    x.%s = new bytes(%i); \n" name (BL8.length bs)
          bytes = mconcat . map (uncurry byte) . zip [0..] . map ord $ BL8.unpack bs
        in
          mconcat [alloc, bytes]
      IntList typ xs ->
        let
          elem :: Int -> Integer -> String
          elem i x = printf "    x.%s[%i] = %i; \n" name i x

          alloc = printf "    x.%s = new %s[](%i); \n" name typ (length xs)
          elems = mconcat . map (uncurry elem) $ zip [0..] xs
        in
          mconcat [alloc, elems]
      BytesList bss -> 
        let
          bytes :: Int -> ByteString -> String
          bytes i bs =
            let
              byte :: Int -> Int -> String
              byte j b = printf "    x.%s[%i][%i] = %i; \n" name i j b

              alloc = printf "    x.%s[%i] = new bytes(%i); \n" name i (BL8.length bs)
              bytes = mconcat . map (uncurry byte) . zip [0..] . map ord $ BL8.unpack bs
            in
              mconcat [alloc, bytes]
          alloc = printf "    x.%s = new bytes[](%i); \n" name (length bss)
          elems = mconcat . map (uncurry bytes) $ zip [0..] bss
        in
          mconcat [alloc, elems]
 
  in return $ format tmpl
    [ ("name",         name)
    , ("lib",          name <> "Codec")
    , ("fieldChecks",  strip . mconcat $ map (uncurry fieldCheck) checks)
    , ("fieldSetters", strip . mconcat $ map (uncurry fieldSet) checks)
    ]

onFailure :: Code -> Code -> String -> String -> String -> Spec Text
onFailure code callData objData testName err = do
  lift . putStrLn $ "Failed: "                 ++ testName
  lift . putStrLn $ "The error was: "          ++ err
  lift . putStrLn $ "Call data: "              ++ (unpack callData)
  lift . putStrLn $ "Object data: "            ++ objData
  lift . putStrLn $ "Test contract source: \n" ++ (unpack code)
  return "Error"
  
