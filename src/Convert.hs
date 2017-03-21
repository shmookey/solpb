{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Convert where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (pack)

import Text.DescriptorProtos.FileDescriptorProto
  (FileDescriptorProto(FileDescriptorProto, message_type))
import Text.DescriptorProtos.FieldDescriptorProto
  (FieldDescriptorProto(FieldDescriptorProto, label, name, number, type', type_name))
import Text.DescriptorProtos.DescriptorProto
  (DescriptorProto(DescriptorProto, field, nested_type))
import Text.ProtocolBuffers.Basic (uToString)
import qualified Text.DescriptorProtos.FieldDescriptorProto as FieldDescriptorProto
import qualified Text.DescriptorProtos.DescriptorProto as DescriptorProto
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type as FieldType
import qualified Text.DescriptorProtos.FieldDescriptorProto.Label as FieldLabel

import Control.Monad.Result

import Gen
import Types
import qualified Library

convert :: FileDescriptorProto -> [(Name, Code)]
convert (FileDescriptorProto {message_type}) =
  let
    protos = map (\(x,kvs) -> (x, (force $ Library.run x kvs)))
             . map extractStruct
             . concat 
             $ fmap allDescriptorProtos message_type
    force (Ok code) = code
  in
    protos

allDescriptorProtos :: DescriptorProto -> [DescriptorProto]
allDescriptorProtos x@(DescriptorProto {field, nested_type}) =
  case toList nested_type of 
    [] -> [x]
    xs -> x:(concat $ map allDescriptorProtos xs)

extractStruct :: DescriptorProto -> (Name, Map Int (Name, FieldType))
extractStruct srcStruct@(DescriptorProto {field=srcFields}) =
  let
    fields = Map.fromList . catMaybes . toList $ fmap fieldProps srcFields

    fieldProps :: FieldDescriptorProto -> Maybe (Int, (Name, FieldType))
    fieldProps src = do
      num   <- fromIntegral       <$> FieldDescriptorProto.number src
      name  <- (pack . uToString) <$> FieldDescriptorProto.name src
      label <- convertLabel       <$> FieldDescriptorProto.label src
      ft    <- return $ getFieldType src label
      return (num, (name, ft))
 
    convertLabel :: FieldLabel.Label -> Label
    convertLabel x = case x of
      FieldLabel.LABEL_OPTIONAL -> Optional
      FieldLabel.LABEL_REQUIRED -> Required
      FieldLabel.LABEL_REPEATED -> Repeated

    getFieldType :: FieldDescriptorProto -> Label -> FieldType
    getFieldType (FieldDescriptorProto {type', type_name}) =
      case (type', type_name) of
        (Just FieldType.TYPE_UINT32, _) -> Prim "uint32"
        (Just FieldType.TYPE_UINT64, _) -> Prim "uint64"
        (Just FieldType.TYPE_STRING, _) -> Ref "string"
        (Just FieldType.TYPE_BYTES, _)  -> Ref "bytes"
        (Nothing, Just x)               -> User . pack $ uToString x

  in case DescriptorProto.name srcStruct of
    Just name  -> (pack $ uToString name, fields)
    _          -> error "Message type must have a name."

