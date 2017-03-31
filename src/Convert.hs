{-# LANGUAGE NamedFieldPuns #-}

module Convert where

import Prelude hiding (fail)
import Data.Foldable (toList)
import Data.List (intercalate, isPrefixOf, nubBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Text (pack, unpack)
import Data.Semigroup ((<>))
import qualified Data.Map as Map

import Text.DescriptorProtos.FileDescriptorProto
  (FileDescriptorProto(FileDescriptorProto, message_type))
import Text.DescriptorProtos.FieldDescriptorProto
  (FieldDescriptorProto(FieldDescriptorProto, label, number, type', type_name))
import Text.DescriptorProtos.DescriptorProto
  (DescriptorProto(DescriptorProto, field, name, nested_type))
import Text.ProtocolBuffers.Basic (Utf8, uToString)
import qualified Text.DescriptorProtos.FieldDescriptorProto as FDP
import qualified Text.DescriptorProtos.DescriptorProto as DP
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type as FT
import qualified Text.DescriptorProtos.FieldDescriptorProto.Label as FL
import qualified Text.ProtocolBuffers.ProtoCompile.Resolve as Resolve

import Control.Monad.Result
import Control.Monad.Resultant

import Types


loadMany :: [FilePath] -> App [Struct]
loadMany paths =
  nubBy (\a -> (==) (structName a) . structName) . concat <$> mapM load paths

load :: FilePath -> App [Struct]
load path =
  let
    path' = Resolve.LocalFP path
  in do
    dirs <- map Resolve.LocalFP . optIncludeDirs <$> getConfig
    fdps <- safeIO (snd <$> Resolve.loadProto dirs path')
    concat <$> mapM collect fdps

collect :: FileDescriptorProto -> App [Struct]
collect (FileDescriptorProto {message_type}) =
  let
    findAll :: Name -> DescriptorProto -> [(Name, DescriptorProto)]
    findAll prefix x@(DescriptorProto {field, nested_type, name}) =
      case (toList nested_type, name) of 
        ([], _) -> [(prefix, x)]
        (xs, p) -> (prefix, x):(concatMap (findAll prefix') xs)
      where
        newPrefix k  = prefix <> k <> pack "_"
        Just prefix' = fmap (newPrefix . toName) name
  in do
    structs <- mapM (uncurry create) . concatMap (findAll mempty) $ toList message_type
    return $ filter (not . null . structFields) structs

create :: Name -> DescriptorProto -> App Struct
create prefix src@(DescriptorProto {field}) =
  let

    getField :: Int -> FieldDescriptorProto -> App Field
    getField i x = do
      num  <- fromIntegral <$> fromMaybe "missing field number" (FDP.number x)
      name <- toName       <$> fromMaybe "missing field name"   (FDP.name   x)
      lbl  <- convertLabel <$> fromMaybe "missing field label"  (FDP.label  x)
      vt   <- getValueType x
      return $ Field num i name vt lbl 
 
    getValueType :: FieldDescriptorProto -> App ValueType
    getValueType (FieldDescriptorProto {type', type_name}) =
      case (type', type_name) of
        (Just FT.TYPE_UINT32,   _)     -> return $ VarintType   UInt32 
        (Just FT.TYPE_UINT64,   _)     -> return $ VarintType   UInt64
        (Just FT.TYPE_INT32,    _)     -> return $ VarintType   Int32
        (Just FT.TYPE_INT64,    _)     -> return $ VarintType   Int64
        (Just FT.TYPE_SINT32,   _)     -> return $ VarintType   SInt32
        (Just FT.TYPE_SINT64,   _)     -> return $ VarintType   SInt64 
        (Just FT.TYPE_BOOL,     _)     -> return $ VarintType   Bool  
        (Just FT.TYPE_FIXED32,  _)     -> return $ Fixed32Type  Fixed32
        (Just FT.TYPE_SFIXED32, _)     -> return $ Fixed32Type  SFixed32
        (Just FT.TYPE_FIXED64,  _)     -> return $ Fixed64Type  Fixed64
        (Just FT.TYPE_SFIXED64, _)     -> return $ Fixed64Type  SFixed64
        (Just FT.TYPE_STRING,   _)     -> return $ LenDelim     String
        (Just FT.TYPE_BYTES,    _)     -> return $ LenDelim     Bytes   
        (Just FT.TYPE_MESSAGE, Just x) -> return $ LenDelim     (messageType $ uToString x)
        (Nothing, Just x)              -> return $ LenDelim     (messageType $ uToString x)
        (Just x, _)                    -> fail $ "unsupported field type: " ++ (show x)
        _                              -> fail "missing field type"

    messageType :: String -> LenDelimType
    messageType name = 
      let
        parts = filter (not . null) $ splitOn "." name
        clean = pack . intercalate "_" $ drop 1 parts
      in case parts of
        ("Solidity":"solidity":x:[]) -> Message . Sol $ pack x
        _ -> Message . User clean $ clean <> pack "Codec"

    convertLabel :: FL.Label -> Label
    convertLabel x = case x of
      FL.LABEL_OPTIONAL -> Optional
      FL.LABEL_REQUIRED -> Required
      FL.LABEL_REPEATED -> Repeated

  in do 
    name   <- toName <$> fromMaybe "missing message type name" (DP.name src)
    fields <- mapM (uncurry getField) (zip [0..] $ toList field)
    return $ Struct (prefix <> name) fields

toName :: Utf8 -> Name
toName = pack . uToString

