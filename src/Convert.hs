{-# LANGUAGE NamedFieldPuns #-}

module Convert where

import Data.Foldable (toList)

import Text.DescriptorProtos.FileDescriptorProto
  (FileDescriptorProto(FileDescriptorProto, message_type))
import Text.DescriptorProtos.FieldDescriptorProto
  (FieldDescriptorProto(FieldDescriptorProto, name, number, type'))
import Text.DescriptorProtos.DescriptorProto
  (DescriptorProto(DescriptorProto, field))
import Text.ProtocolBuffers.Basic (uToString)
import qualified Text.DescriptorProtos.DescriptorProto as DescriptorProto
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type as FieldType

import qualified Solidity as S



convert :: FileDescriptorProto -> [String]
convert (FileDescriptorProto {message_type}) =
  toList $ fmap (S.formatLibrary . createStruct) message_type

createStruct :: DescriptorProto -> S.Struct
createStruct msg@(DescriptorProto {field=dFields}) =
  let
    sFields = toList $ fmap mkField dFields

    mkField :: FieldDescriptorProto -> S.Field
    mkField (FieldDescriptorProto {number, name, type'}) = 
      case (number, name, type') of
        (Just i, Just x, Just t) -> S.Field (fromIntegral i) (uToString x) (convertType t)
        _                        -> error "Invalid field."

  in case DescriptorProto.name msg of
    Just sName -> S.Struct (uToString sName) sFields
    _          -> error "Message type must have a name."

convertType :: FieldType.Type -> S.Type
convertType x = case x of
  FieldType.TYPE_UINT32 -> S.UInt32
  FieldType.TYPE_UINT64 -> S.UInt64
  FieldType.TYPE_STRING -> S.String
