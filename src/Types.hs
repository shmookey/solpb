{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as T

import Control.Monad.Resultant


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

data State = State
  { stName   :: Name
  , stFields :: Map Int (Name, FieldType)
  }

type Generator = Resultant State String



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


-- Monad access helpers

addField :: Int -> Name -> FieldType -> Generator ()
addField i k ft = updateFields $ Map.insert i (k, ft)

-- Basic monad access operations

getName :: Generator Name
getName = stName <$> getState

getFields :: Generator (Map Int (Name, FieldType))
getFields = stFields <$> getState

setFields :: Map Int (Name, FieldType) -> Generator ()
setFields x = updateState $ \st -> st { stFields = x }

updateFields :: (Map Int (Name, FieldType) -> Map Int (Name, FieldType)) -> Generator ()
updateFields f = getFields >>= setFields . f

