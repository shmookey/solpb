{-# LANGUAGE NamedFieldPuns #-}

module Convert where

import Prelude hiding (fail)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Text (pack, unpack)
import qualified Data.Map as Map

import Text.DescriptorProtos.FileDescriptorProto
  (FileDescriptorProto(FileDescriptorProto, message_type))
import Text.DescriptorProtos.FieldDescriptorProto
  (FieldDescriptorProto(FieldDescriptorProto, label, name, number, type', type_name))
import Text.DescriptorProtos.DescriptorProto
  (DescriptorProto(DescriptorProto, field, nested_type))
import Text.ProtocolBuffers.Basic (Utf8, uToString)
import qualified Text.DescriptorProtos.FieldDescriptorProto as FDP
import qualified Text.DescriptorProtos.DescriptorProto as DP
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type as FT
import qualified Text.DescriptorProtos.FieldDescriptorProto.Label as FL

import Control.Monad.Result
import Control.Monad.Resultant

import Types


collect :: FileDescriptorProto -> App [Struct]
collect (FileDescriptorProto {message_type}) =
  let
    findAll :: DescriptorProto -> [DescriptorProto]
    findAll x@(DescriptorProto {field, nested_type}) =
      case toList nested_type of 
        [] -> [x]
        xs -> x:(concatMap findAll xs)
  in
    mapM create . concatMap findAll $ toList message_type

create :: DescriptorProto -> App Struct
create src@(DescriptorProto {field}) =
  let
    fieldProps :: FieldDescriptorProto -> App (Int, (Name, FieldType))
    fieldProps src = do
      num  <- fromIntegral <$> fromMaybe "missing field number" (FDP.number src)
      name <- toName       <$> fromMaybe "missing field name"   (FDP.name src)
      lbl  <- convertLabel <$> fromMaybe "missing field label"  (FDP.label src)
      ft   <- getFieldType src lbl
      return (num, (name, ft))
 
    getFieldType :: FieldDescriptorProto -> Label -> App FieldType
    getFieldType (FieldDescriptorProto {type', type_name}) label =
      case (type', type_name) of
        (Just FT.TYPE_UINT32, _) -> return $ Prim (pack "uint32") label
        (Just FT.TYPE_UINT64, _) -> return $ Prim (pack "uint64") label
        (Just FT.TYPE_STRING, _) -> return $ Ref (pack "string") label
        (Just FT.TYPE_BYTES, _)  -> return $ Ref (pack "bytes") label
        (Just x, _)              -> fail $ "unsupported field type: " ++ (show x)
        (Nothing, Just x)        -> readCustomType (toName x) label
        _                        -> fail "missing field type"

    readCustomType :: Name -> Label -> App FieldType
    readCustomType typName lbl = case (splitOn "." $ unpack typName) of
      []              -> fail "missing field type name"
      (x:[])          -> return $ User (pack x) lbl
      ("solidity":xs) -> return $ Sol (pack $ intercalate "." xs) lbl
      _               -> fail "user-defined nested types not yet supported"

    convertLabel :: FL.Label -> Label
    convertLabel x = case x of
      FL.LABEL_OPTIONAL -> Optional
      FL.LABEL_REQUIRED -> Required
      FL.LABEL_REPEATED -> Repeated

    toName :: Utf8 -> Name
    toName = pack . uToString

  in do 
    name   <- toName <$> fromMaybe "missing message type name" (DP.name src)
    fields <- Map.fromList <$> mapM fieldProps (toList field)
    return $ Struct name fields

