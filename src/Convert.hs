{-# LANGUAGE NamedFieldPuns #-}

module Convert where

import Data.Foldable (toList)

import Text.DescriptorProtos.FileDescriptorProto
  (FileDescriptorProto(FileDescriptorProto, message_type))
import Text.DescriptorProtos.FieldDescriptorProto
  (FieldDescriptorProto(FieldDescriptorProto, label, name, number, type', type_name))
import Text.DescriptorProtos.DescriptorProto
  (DescriptorProto(DescriptorProto, field, nested_type))
import Text.ProtocolBuffers.Basic (uToString)
import qualified Text.DescriptorProtos.DescriptorProto as DescriptorProto
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type as FieldType
import qualified Text.DescriptorProtos.FieldDescriptorProto.Label as FieldLabel

import qualified Solidity as S



convert :: FileDescriptorProto -> [String]
convert (FileDescriptorProto {message_type}) =
  let
    protos = concat $ fmap allDescriptorProtos message_type
  in
    toList $ fmap (S.formatLibrary . createStruct) protos

allDescriptorProtos :: DescriptorProto -> [DescriptorProto]
allDescriptorProtos x@(DescriptorProto {field, nested_type}) =
  case toList nested_type of [] -> [x]
                             xs -> x:(concat $ map allDescriptorProtos xs)

createStruct :: DescriptorProto -> S.Struct
createStruct msg@(DescriptorProto {field=dFields}) =
  let
    sFields = toList $ fmap mkField dFields

    mkField :: FieldDescriptorProto -> S.Field
    mkField fld@(FieldDescriptorProto {number, name, type', label}) = 
      case (number, name, label) of
        (Just i, Just x, Just l) ->
          S.Field (fromIntegral i) (uToString x) (convertType fld) (convertLabel l)
        _ ->
          error "Invalid field."

  in case DescriptorProto.name msg of
    Just sName -> S.Struct (uToString sName) sFields
    _          -> error "Message type must have a name."

convertType :: FieldDescriptorProto -> S.Type
convertType (FieldDescriptorProto {type', type_name}) =
  case (type', type_name) of
    (Just FieldType.TYPE_UINT32, _) -> S.UInt32
    (Just FieldType.TYPE_UINT64, _) -> S.UInt64
    (Just FieldType.TYPE_STRING, _) -> S.String
    (Nothing, Just x)               -> S.Message $ uToString x

convertLabel :: FieldLabel.Label -> S.Label
convertLabel x = case x of
  FieldLabel.LABEL_OPTIONAL -> S.Optional
  FieldLabel.LABEL_REQUIRED -> S.Required
  FieldLabel.LABEL_REPEATED -> S.Repeated

