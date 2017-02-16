module Solidity where

import qualified Data.Map as Map
import Text.Printf (printf)
import Data.List (intercalate)
import Data.Map (Map)

data Type
  = UInt | UInt32 | UInt64
  | String
  deriving (Eq, Ord)

type FieldName  = String
type StructName = String

data Field  = Field Int String Type
data Setter = Setter StructName Type [Field]
data Struct = Struct StructName [Field]

class Format x where
  fmt :: x -> String

instance Format Type where
  fmt x = case x of
    UInt   -> "uint"
    UInt32 -> "uint32"
    UInt64 -> "uint64"
    String -> "string"

instance Format Setter where
  fmt (Setter structName setterType fields) =
    let
      typ     = fmt setterType
      name    = "setField_" ++ typ
      args    = printf "(%s x, uint fieldId, %s obj)" typ structName :: String
      proto   = printf "function %s%s internal constant" name args :: String
      ifs     = case fields of 
                 [] -> "throw;"
                 _  -> (intercalate " else " $ map iff fields) ++ term
      term    = " else {\n  throw;\n}"

      iff :: Field -> String
      iff (Field i k _) = printf "if (%s) {\n  %s\n}" cond action
        where cond   = (printf "fieldId == %i" i) :: String
              action = (printf "obj.%s = x;" k) :: String

    in
      printf "%s {\n%s}" proto (indent 2 ifs)

instance Format Struct where
  fmt (Struct name fields) =
    let
      fieldLines = indent 2 . intercalate "\n" $ map field fields

      field :: Field -> String
      field (Field _ k t) = printf "%s %s;" (fmt t) k

    in
      printf "struct %s {\n%s}" name fieldLines

formatLibrary :: Struct -> String
formatLibrary x@(Struct name fields) =
  let
    decoder = indent 2 $ formatDecoder name
    statics = indent 2 staticCode
    setters = indent 2 . intercalate "\n" . map fmt $ createSetters name fields
    struct  = indent 2 $ fmt x
    ftypes  = indent 2 $ formatGetFieldType x
  in
    printf "library %s {\n%s\n%s\n%s\n%s\n%s\n}" name struct ftypes statics decoder setters

createSetters :: StructName -> [Field] -> [Setter]
createSetters name =
  let
    empties = Map.fromList $ zip [UInt, UInt32, UInt64, String] (repeat [])

    accumulate :: Map Type [Field] -> Field -> Map Type [Field]
    accumulate acc x@(Field i k t) = case Map.lookup t acc of
      Just xs -> Map.insert t (x:xs) acc
      Nothing -> Map.insert t [x] acc
  in
    map (uncurry $ Setter name) . Map.toList . foldl accumulate empties

indent :: Int -> String -> String
indent n = unlines . map (spaces ++) . lines
  where spaces = take n $ repeat ' '

formatGetFieldType :: Struct -> String
formatGetFieldType (Struct name fields) =
  let
    proto   = "function fieldType(uint id) internal constant returns (SolType)"
    ifs     = (intercalate " else " $ map iff fields) ++ term
    term    = " else {\n  throw;\n}"

    iff :: Field -> String
    iff (Field i _ t) = printf "if (%s) {\n  %s\n}" cond action
      where cond   = printf "id == %i" i :: String
            action = printf "return SolType.%s;" (solEnum t) :: String

  in
    printf "%s {\n%s}" proto (indent 2 ifs)

solEnum :: Type -> String
solEnum x = case x of
  UInt   -> "UInt"
  UInt32 -> "UInt32"
  UInt64 -> "UInt64"
  String -> "String"

formatDecoder :: StructName -> String
formatDecoder t = printf "\
  \function decode%s(bytes data) internal constant returns (%s) {               \n\
  \  %s memory buf;                                                             \n\
  \                                                                             \n\
  \  // The size of `data` and its first element are offset by a 256-bit length \n\
  \  uint len = data.length + 32;                                               \n\
  \  uint ptr = 32;                                                             \n\
  \  uint bytesRead;                                                            \n\
  \                                                                             \n\
  \  while (ptr < len) {                                                        \n\
  \    uint fieldId;                                                            \n\
  \    WireType wireType;                                                       \n\
  \    (fieldId, wireType, bytesRead) = readKey(ptr, data);                     \n\
  \    SolType typ = fieldType(fieldId);                                        \n\
  \    ptr += bytesRead;                                                        \n\
  \                                                                             \n\
  \    if(wireType == WireType.Varint) {                                        \n\
  \      uint x1;                                                               \n\
  \      (x1, bytesRead) = readVarInt(ptr, data);                               \n\
  \      ptr += bytesRead;                                                      \n\
  \      if (typ == SolType.UInt) {                                             \n\
  \        setField_uint(x1, fieldId, buf);                                     \n\
  \      } else if (typ == SolType.UInt32) {                                    \n\
  \        setField_uint32(uint32(x1), fieldId, buf);                           \n\
  \      } else if (typ == SolType.UInt64) {                                    \n\
  \        setField_uint64(uint64(x1), fieldId, buf);                           \n\
  \      } else {                                                               \n\
  \        throw;                                                               \n\
  \      }                                                                      \n\
  \                                                                             \n\
  \    } else if(wireType == WireType.Fixed64) {                                \n\
  \      uint64 x2 = readUInt64(ptr, data);                                     \n\
  \      ptr += 8;                                                              \n\
  \      setField_uint64(x2, fieldId, buf);                                     \n\
  \                                                                             \n\
  \    } else if(wireType == WireType.LengthDelim) {                            \n\
  \      uint dataLen;                                                          \n\
  \      (dataLen, bytesRead) = readVarInt(ptr, data);                          \n\
  \      ptr += bytesRead;                                                      \n\
  \      if (typ == SolType.String) {                                           \n\
  \        string memory xs = readString(ptr, data, dataLen);                   \n\
  \        setField_string(xs, fieldId, buf);                                   \n\
  \      } else {                                                               \n\
  \        throw;                                                               \n\
  \      }                                                                      \n\
  \      ptr += dataLen;                                                        \n\
  \                                                                             \n\
  \    } else if(wireType == WireType.StartGroup) {                             \n\
  \      throw; // Deprecated, not implemented                                  \n\
  \                                                                             \n\
  \    } else if(wireType == WireType.EndGroup) {                               \n\
  \      throw; // Deprecated, not implemented                                  \n\
  \                                                                             \n\
  \    } else if(wireType == WireType.Fixed32) {                                \n\
  \      uint32 x3 = readUInt32(ptr, data);                                     \n\
  \      ptr += 4;                                                              \n\
  \      setField_uint32(x3, fieldId, buf);                                     \n\
  \    }                                                                        \n\
  \  }                                                                          \n\
  \  return buf;                                                                \n\
  \}" t t t

staticCode :: String
staticCode = "\
  \enum WireType { Varint, Fixed64, LengthDelim, StartGroup, EndGroup, Fixed32 }                \n\
  \enum SolType { UInt, UInt32, UInt64, String }                                                \n\
  \                                                                                             \n\
  \function readKey(uint ptr, bytes data) constant returns (uint, WireType, uint) {             \n\
  \  var (x, n) = readVarInt(ptr, data);                                                        \n\
  \  WireType typeId  = WireType(x & 7);                                                        \n\
  \  uint fieldId = x / 8; //x >> 3;                                                            \n\
  \  return (fieldId, typeId, n);                                                               \n\
  \}                                                                                            \n\
  \                                                                                             \n\
  \function readString(uint ptr, bytes data, uint length) constant returns (string) {           \n\
  \  bytes memory b = new bytes(length);                                                        \n\
  \  assembly {                                                                                 \n\
  \    let bptr  := add(b, 32)                                                                  \n\
  \    let count := 0                                                                           \n\
  \    ptr       := add(data, ptr)                                                              \n\
  \    loop :                                                                                   \n\
  \      jumpi(end, eq(count, length))                                                          \n\
  \      mstore8(bptr, byte(0, mload(ptr)))                                                     \n\
  \      ptr   := add(ptr, 1)                                                                   \n\
  \      bptr  := add(bptr, 1)                                                                  \n\
  \      count := add(count, 1)                                                                 \n\
  \      jump(loop)                                                                             \n\
  \    end:                                                                                     \n\
  \  }                                                                                          \n\
  \  return string(b);                                                                          \n\
  \}                                                                                            \n\
  \                                                                                             \n\
  \function readVarInt(uint ptr, bytes data) constant returns (uint, uint) {                    \n\
  \  uint x = 0;                                                                                \n\
  \  uint bytesUsed = 0;                                                                        \n\
  \  assembly {                                                                                 \n\
  \    let b := 0                                                                               \n\
  \    ptr   := add(data, ptr)                                                                  \n\
  \    loop :                                                                                   \n\
  \      b         := byte(0, mload(ptr))                                                       \n\
  \      x         := or(x, mul(and(0x7f, b), exp(2, mul(7, bytesUsed))))                       \n\
  \      bytesUsed := add(bytesUsed, 1)                                                         \n\
  \      ptr       := add(ptr, 0x01)                                                            \n\
  \      jumpi(loop, eq(0x80, and(b, 0x80)))                                                    \n\
  \  }                                                                                          \n\
  \  return (x, bytesUsed);                                                                     \n\
  \}                                                                                            \n\
  \                                                                                             \n\
  \function readUInt32(uint ptr, bytes data) constant returns (uint32) {                        \n\
  \  return uint32(readUint(ptr, data, 4));                                                     \n\
  \}                                                                                            \n\
  \                                                                                             \n\
  \function readUInt64(uint ptr, bytes data) constant returns (uint64) {                        \n\
  \  return uint64(readUint(ptr, data, 8));                                                     \n\
  \}                                                                                            \n\
  \                                                                                             \n\
  \function readUint(uint ptr, bytes data, uint bytesLength) constant internal returns (uint) { \n\
  \  uint x = 0;                                                                                \n\
  \  assembly {                                                                                 \n\
  \    let i := 0                                                                               \n\
  \    ptr   := add(data, ptr)                                                                  \n\
  \    loop :                                                                                   \n\
  \      jumpi(end, eq(i, bytesLength))                                                         \n\
  \      x   := or(x, mul(byte(0, mload(ptr)), exp(2, mul(8, i))))                              \n\
  \      ptr := add(ptr, 0x01)                                                                  \n\
  \      i   := add(i, 1)                                                                       \n\
  \      jump(loop)                                                                             \n\
  \    end :                                                                                    \n\
  \  }                                                                                          \n\
  \  return x;                                                                                  \n\
  \}                                                                                            \n\
  \"

