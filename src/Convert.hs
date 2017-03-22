{-# LANGUAGE NamedFieldPuns #-}

module Convert where

import Prelude hiding (fail)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Text (pack)
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


collect :: FileDescriptorProto -> Result String [State]
collect (FileDescriptorProto {message_type}) =
  let
    findAll :: DescriptorProto -> [DescriptorProto]
    findAll x@(DescriptorProto {field, nested_type}) =
      case toList nested_type of 
        [] -> [x]
        xs -> x:(concatMap findAll xs)
  in
    mapM create . concatMap findAll $ toList message_type

create :: DescriptorProto -> Result String State
create src@(DescriptorProto {field}) =
  let
    fieldProps :: FieldDescriptorProto -> Result String (Int, (Name, FieldType))
    fieldProps src = do
      num  <- fromIntegral <$> fromMaybe "missing field number" (FDP.number src)
      name <- toName       <$> fromMaybe "missing field name"   (FDP.name src)
      lbl  <- convertLabel <$> fromMaybe "missing field label"  (FDP.label src)
      ft   <- getFieldType src lbl
      return (num, (name, ft))
 
    getFieldType :: FieldDescriptorProto -> Label -> Result String FieldType
    getFieldType (FieldDescriptorProto {type', type_name}) label =
      case (type', type_name) of
        (Just FT.TYPE_UINT32, _) -> return $ Prim (pack "uint32") label
        (Just FT.TYPE_UINT64, _) -> return $ Prim (pack "uint64") label
        (Just FT.TYPE_STRING, _) -> return $ Ref (pack "string") label
        (Just FT.TYPE_BYTES, _)  -> return $ Ref (pack "bytes") label
        (Just x, _)              -> fail $ "unsupported field type: " ++ (show x)
        (Nothing, Just x)        -> return $ User (toName x) label
        _                        -> fail "missing field type"

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
    return $ State name fields



