module Gen where

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad.Resultant
import Types

type Gen = Resultant State Error

data Error
  = UnresolvedType Name

data State = State
  { stStructs    :: Map Name Struct
  , stName       :: Name
  , stFields     :: Map Int (Name, FieldType)
  }


-- Formatting utils



-- Monad access helpers

addStruct :: Struct -> Gen ()
addStruct x = updateStructs $ Map.insert (structName x) x

getStruct :: Name -> Gen Struct
getStruct x = do
  s <- Map.lookup x <$> getStructs
  fromMaybe (UnresolvedType x) s


-- Basic monad access operations

getName :: Gen Name
getName = stName <$> getState

getStructs :: Gen (Map Name Struct)
getStructs = stStructs <$> getState

getFields :: Gen (Map Int (Name, FieldType))
getFields = stFields <$> getState

setStructs :: Map Name Struct -> Gen ()
setStructs x = updateState $ \st -> st { stStructs = x }

updateStructs :: (Map Name Struct -> Map Name Struct) -> Gen ()
updateStructs f = getStructs >>= setStructs . f

