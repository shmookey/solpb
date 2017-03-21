{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Text as T
import Data.Text (Text)


type Name = Text
type Code = Text

data FieldType
  = Prim Name Label
  | Ref  Name Label
  | User Name Label
  deriving (Show)

data Label
  = Optional 
  | Required 
  | Repeated
  deriving (Show)

data Struct = Struct
  { structName   :: Name
  , structFields :: [Field]
  }

data Field = Field 
  { fieldNumber :: Int
  , fieldName   :: String
  }

-- | Qualify struct name with its containing library
qualifiedType :: Name -> Name
qualifiedType x = T.concat [libraryName x, ".", x]

-- | Library name for a given struct
libraryName :: Name -> Name
libraryName x = T.append x "Codec"

-- | Scalar type to use for function locals
localScalar :: FieldType -> Name
localScalar ft = case ft of
  Prim x _ -> x
  Ref  x _ -> T.append x " memory"
  User x _ -> T.append (qualifiedType x) " memory"

-- | Type of a field, e.g. "uint32[]" or "FooCodec.Bar"
fieldType :: FieldType -> Name
fieldType ft = 
  let suffix = if isRepeated ft then "[]" else ""
      typ    = fieldTypeName ft
  in flip T.append suffix $ case isStruct ft of
    True -> qualifiedType typ
    False -> typ

fieldLabel :: FieldType -> Label
fieldLabel ft = case ft of
  Prim _ x -> x
  Ref  _ x -> x
  User _ x -> x

fieldTypeName :: FieldType -> Name
fieldTypeName ft = case ft of
  Prim x _ -> x
  Ref x _  -> x
  User x _ -> x

fieldLibrary :: FieldType -> Name
fieldLibrary ft = case ft of
  Prim x _ -> ""
  Ref x _  -> ""
  User x _ -> libraryName x


isRepeated :: FieldType -> Bool
isRepeated ft = case fieldLabel ft of
  Repeated -> True
  _        -> False

isStruct :: FieldType -> Bool
isStruct ft = case ft of
  User _ _ -> True
  _        -> False


