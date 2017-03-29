{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Map (Map)
import Data.Text (Text)
import Data.Semigroup ((<>))
import qualified Data.Map as Map
import qualified Data.Text as T

import Control.Monad.Resultant


type App = ResultantT IO () String

type Name  = Text
type Code  = Text
data Field = Field
  { fieldID    :: Int
  , fieldName  :: Name
  , fieldType  :: ValueType
  , fieldLabel :: Label
  } deriving (Show)

data Label
  = Optional 
  | Required 
  | Repeated
  deriving (Show)

data Struct = Struct
  { structName   :: Name
  , structFields :: [Field]
  }

data Options = Options
  { optDir    :: FilePath
  , optSuffix :: String
  , optPragma :: Bool
  , optInputs :: [FilePath]
  } deriving (Show)

data ValueType
  = VarintType   VarintType
  | Fixed64Type  Fixed64Type
  | LenDelim LenDelimType
  | Fixed32Type  Fixed32Type
  | StartGroup
  | EndGroup
  deriving (Eq, Show)

data VarintType
  = Int32  | Int64  | UInt32 | UInt64
  | SInt32 | SInt64 | Bool   | Enum 
  deriving (Eq, Show) 

data Fixed64Type
  = Fixed64 | SFixed64 | Double
  deriving (Eq, Show) 

data LenDelimType
  = String | Bytes | Message Message | Packed
  deriving (Eq, Show) 

data Fixed32Type
  = Fixed32 | SFixed32 | Float
  deriving (Eq, Show) 

data Message
  = User Name Name -- Basic type name, library name
  | Sol Name       -- Native type name
  deriving (Eq, Show)

data TypeContext
  = Scalar | Normal | Quote
  deriving (Eq, Show)

formatValueType :: TypeContext -> ValueType -> Name
formatValueType ctx typ =
  let
    formatMessageType :: Message -> Name
    formatMessageType msg = case (msg, ctx) of
      (User name lib, Quote) -> lib <> "_" <> name
      (User name lib, _)     -> lib <> "." <> name
      (Sol name, _)          -> name

  in case typ of
    VarintType x -> case x of
      Int32  -> "int32"
      Int64  -> "int64"
      UInt32 -> "uint32"
      UInt64 -> "uint64"
      SInt32 -> "int32"
      SInt64 -> "int64"
      Bool   -> "bool"
      Enum   -> error "internal error: enums not yet supported"
  
    Fixed64Type x -> case x of
      Fixed64  -> "int64"
      SFixed64 -> "int64"
      Double   -> error "internal error: floating point numbers not supported"
  
    LenDelim x -> case x of
      String    -> "string"
      Bytes     -> "bytes"
      Message m -> formatMessageType m
      Packed    -> error "internal error: packed encodings not yet supported"
  
    Fixed32Type x -> case x of
      Fixed32  -> "uint32"
      SFixed32 -> "int32"
      Float    -> error "internal error: floating point numbers not supported"
  
    StartGroup -> error "internal error: groups not supported"
    EndGroup   -> error "internal error: groups not supported"

formatFieldType :: TypeContext -> Field -> Name
formatFieldType ctx fld =
  let
    base = formatValueType ctx $ fieldType fld
  in
    case (ctx, isRepeated fld) of
      (Normal, True) -> base <> "[]"
      _              -> base

formatEncoding :: ValueType -> Name
formatEncoding typ = case typ of
  VarintType x -> case x of
    Int32  -> "int32"
    Int64  -> "int64"
    UInt32 -> "uint32"
    UInt64 -> "uint64"
    SInt32 -> "sint32"
    SInt64 -> "sint64"
    Bool   -> "bool"
    Enum   -> error "internal error: enums not yet supported"

  Fixed64Type x -> case x of
    Fixed64  -> "fixed64"
    SFixed64 -> "sfixed64"
    Double   -> error "internal error: floating point numbers not supported"

  LenDelim x -> case x of
    Message (Sol x)    -> "sol_" <> x
    Message (User x _) -> x
    String -> "string"
    Bytes  -> "bytes"
    Packed -> error "internal error: packed encodings not yet supported"

  Fixed32Type x -> case x of
    Fixed32  -> "fixed32"
    SFixed32 -> "sfixed32"
    Float    -> error "internal error: floating point numbers not supported"

  StartGroup -> error "internal error: groups not supported"
  EndGroup   -> error "internal error: groups not supported"

formatWireType :: ValueType -> Name
formatWireType typ = case typ of
  VarintType _   -> "_pb._WireType.Varint"
  Fixed64Type _  -> "_pb._WireType.Fixed64"
  LenDelim _     -> "_pb._WireType.LengthDelim"
  Fixed32Type _  -> "_pb._WireType.Fixed32"
  StartGroup     -> error "internal error: groups not supported"
  EndGroup       -> error "internal error: groups not supported"

formatLibrary :: ValueType -> Name
formatLibrary typ = case typ of
  LenDelim (Message (User _ x)) -> x
  _                             -> ""

isRepeated :: Field -> Bool
isRepeated fld = case fieldLabel fld of
  Repeated -> True
  _        -> False

isStruct :: Field -> Bool
isStruct fld = case fieldType fld of
  LenDelim (Message (User _ _)) -> True
  _                             -> False

