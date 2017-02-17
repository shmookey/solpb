module Solidity where

import qualified Data.Map as Map
import Text.Printf (printf)
import Data.List (intercalate, partition)
import Data.Map (Map)

data Type = UInt | UInt32 | UInt64 | String
  deriving (Eq, Ord)

data Label = Optional | Required | Repeated
  deriving (Eq)

type FieldName  = String
type StructName = String

data Field  = Field Int String Type Label
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
      iff (Field i k _ _) = printf "if (%s) {\n  %s\n}" cond action
        where cond   = (printf "fieldId == %i" i) :: String
              action = (printf "obj.%s = x;" k) :: String

    in
      printf "%s {\n%s}" proto (indent 2 ifs)

instance Format Struct where
  fmt (Struct name fields) =
    let
      fieldLines = indent 2 . intercalate "\n" $ map field fields

      field :: Field -> String
      field (Field _ k t r) = case r of
        Repeated -> printf "%s[] %s;" (fmt t) k
        _        -> printf "%s %s;" (fmt t) k

    in
      printf "struct %s {\n%s}" name fieldLines

formatLibrary :: Struct -> String
formatLibrary x@(Struct name fields) =
  let
    decoder  = indent 2 $ formatDecoder name (zip arrays [0..])
    statics  = indent 2 staticCode
    setters  = indent 2 $ createSetters name fields
    struct   = indent 2 $ fmt x
    ftypes   = indent 2 $ formatGetFieldType x
    counters = indent 2 $ formatGetRepeatCounter x counterIds

    (scalars,arrays) = partition isScalar fields
    counterIds       = (zip arrays [0..]) ++ (zip scalars $ repeat (-1))
  in
    printf "library %s {\n%s\n%s\n%s\n%s\n%s\n%s\n}" name struct decoder counters ftypes statics setters

createSetters :: StructName -> [Field] -> String
createSetters name fields =
  let
    empties = Map.fromList $ zip [UInt, UInt32, UInt64, String] (repeat [])

    (scalars,arrays) = partition isScalar fields

    accumulate :: Map Type [Field] -> Field -> Map Type [Field]
    accumulate acc x@(Field i k t _) = case Map.lookup t acc of
      Just xs -> Map.insert t (x:xs) acc
      Nothing -> Map.insert t [x] acc

    scalarSetters = map fmt . map (uncurry $ Setter name) . Map.toList . foldl accumulate empties $ scalars
    arraySetters  = map formatArraySetter . map (uncurry $ Setter name) . Map.toList . foldl accumulate empties $ arrays
  in
    intercalate "\n" $ scalarSetters ++ arraySetters

isScalar :: Field -> Bool    
isScalar (Field _ _ _ x) = not (x == Repeated)

formatGetRepeatCounter :: Struct -> [(Field, Int)] -> String
formatGetRepeatCounter (Struct name fields) counterIds =
  let
    proto            = "function repeatCounter(uint fieldId) internal constant returns (int)"
    ifs              = (intercalate " else " $ map iff counterIds) ++ term
    term             = " else {\n  throw;\n}"

    iff :: (Field, Int) -> String
    iff (Field i _ _ _, c) = printf "if (%s) {\n  %s\n}" cond action
      where cond   = (printf "fieldId == %i" i) :: String
            action = (printf "return %i;" c) :: String
  
  in
    printf "%s {\n%s}" proto (indent 2 ifs)
    
    

formatArraySetter :: Setter -> String
formatArraySetter (Setter structName setterType fields) =
  let
    typ     = fmt setterType
    name    = "setArrayField_" ++ typ
    args    = printf "(uint slot, %s x, uint fieldId, %s obj)" typ structName :: String
    proto   = printf "function %s%s internal constant" name args :: String
    ifs     = case fields of 
               [] -> "throw;"
               _  -> (intercalate " else " $ map iff fields) ++ term
    term    = " else {\n  throw;\n}"

    iff :: Field -> String
    iff (Field i k _ _) = printf "if (%s) {\n  %s\n}" cond action
      where cond   = (printf "fieldId == %i" i) :: String
            action = (printf "obj.%s[slot] = x;" k) :: String

  in
    printf "%s {\n%s}" proto (indent 2 ifs)

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
    iff (Field i _ t _) = printf "if (%s) {\n  %s\n}" cond action
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

formatDecoder :: StructName -> [(Field, Int)] -> String
formatDecoder name counterIds = 
  let
    alloc :: (Field, Int) -> String
    alloc (Field _ k t _, i) = printf "buf.%s = new %s[](counters[0][%i]);" k (fmt t) i

    allocs = indent 2 . intercalate "\n" $ map alloc counterIds
  in printf "\
  \function decode%s(bytes data) internal constant returns (%s) {                           \n\
  \  %s memory buf;                                                                         \n\
  \  // The size of `data` and its first element are offset by a 256-bit length             \n\
  \  uint[2][2] memory counters = [[uint(0),uint(0)],[uint(0),uint(0)]];                    \n\
  \                                                                                         \n\
  \  // Declare these in advance to save stack space                                        \n\
  \  uint ptr;                                                                              \n\
  \  uint resultUInt = 0;                                                                   \n\
  \  string memory resultString;                                                            \n\
  \  uint dataLen = 0;                                                                      \n\
  \  uint bytesRead;                                                                        \n\
  \  int counterId;                                                                         \n\
  \  WireType wireType;                                                                     \n\
  \  SolType solType;                                                                       \n\
  \  uint fieldId;                                                                          \n\
  \  uint slot;                                                                             \n\
  \                                                                                         \n\
  \  /* First pass: count instances of repeated values, read and store non-repeated values  \n\
  \  */                                                                                     \n\
  \                                                                                         \n\
  \  ptr = 32;                                                                              \n\
  \  while (ptr < data.length + 32) {                                                       \n\
  \    (fieldId, wireType, bytesRead) = readKey(ptr, data);                                 \n\
  \    solType                        = fieldType(fieldId);                                 \n\
  \    ptr                           += bytesRead;                                          \n\
  \    counterId                      = repeatCounter(fieldId);                             \n\
  \                                                                                         \n\
  \    if (counterId == -1) {                                                               \n\
  \      // Non-repeated value. Read and store as usual.                                    \n\
  \                                                                                         \n\
  \      if(wireType == WireType.Varint) {                                                  \n\
  \        (resultUInt, bytesRead) = readVarInt(ptr, data);                                 \n\
  \        ptr += bytesRead;                                                                \n\
  \        if (solType == SolType.UInt) {                                                   \n\
  \          setField_uint(resultUInt, fieldId, buf);                                       \n\
  \        } else if (solType == SolType.UInt32) {                                          \n\
  \          setField_uint32(uint32(resultUInt), fieldId, buf);                             \n\
  \        } else if (solType == SolType.UInt64) {                                          \n\
  \          setField_uint64(uint64(resultUInt), fieldId, buf);                             \n\
  \        } else {                                                                         \n\
  \          throw;                                                                         \n\
  \        }                                                                                \n\
  \      } else if(wireType == WireType.Fixed64) {                                          \n\
  \        resultUInt = uint(readUInt64(ptr, data));                                        \n\
  \        ptr += 8;                                                                        \n\
  \        setField_uint64(uint64(resultUInt), fieldId, buf);                               \n\
  \      } else if(wireType == WireType.LengthDelim) {                                      \n\
  \        (dataLen, bytesRead) = readVarInt(ptr, data);                                    \n\
  \        ptr += bytesRead;                                                                \n\
  \        if (solType == SolType.String) {                                                 \n\
  \          resultString = readString(ptr, data, dataLen);                                 \n\
  \          setField_string(resultString, fieldId, buf);                                   \n\
  \        } else {                                                                         \n\
  \          throw;                                                                         \n\
  \        }                                                                                \n\
  \        ptr += dataLen;                                                                  \n\
  \      } else if(wireType == WireType.StartGroup) {                                       \n\
  \        throw; // Deprecated, not implemented                                            \n\
  \      } else if(wireType == WireType.EndGroup) {                                         \n\
  \        throw; // Deprecated, not implemented                                            \n\
  \      } else if(wireType == WireType.Fixed32) {                                          \n\
  \        resultUInt = uint(readUInt32(ptr, data));                                        \n\
  \        ptr += 4;                                                                        \n\
  \        setField_uint32(uint32(resultUInt), fieldId, buf);                               \n\
  \      }                                                                                  \n\
  \                                                                                         \n\
  \    } else {                                                                             \n\
  \      // Repeated value. Update instance counter.                                        \n\
  \      // We still have to read most values but we don't yet have anywhere to store them. \n\
  \      if (wireType == WireType.Varint) {                                                 \n\
  \        (resultUInt, bytesRead) = readVarInt(ptr, data);                                 \n\
  \        ptr += bytesRead;                                                                \n\
  \        counters[0][uint(counterId)] += 1;                                               \n\
  \      } else if (wireType == WireType.Fixed64) {                                         \n\
  \        ptr += 8;                                                                        \n\
  \        counters[0][uint(counterId)] += 1;                                               \n\
  \      } else if (wireType == WireType.LengthDelim) {                                     \n\
  \        (resultUInt, bytesRead) = readVarInt(ptr, data);                                 \n\
  \        ptr += bytesRead + resultUInt;                                                   \n\
  \        counters[0][uint(counterId)] += 1;                                               \n\
  \      } else if (wireType == WireType.StartGroup) {                                      \n\
  \        throw;                                                                           \n\
  \      } else if (wireType == WireType.EndGroup) {                                        \n\
  \        throw;                                                                           \n\
  \      } else if (wireType == WireType.Fixed32) {                                         \n\
  \        ptr += 4;                                                                        \n\
  \        counters[0][uint(counterId)] += 1;                                               \n\
  \      }                                                                                  \n\
  \    }                                                                                    \n\
  \  }                                                                                      \n\
  \                                                                                         \n\
  \  /* Second pass: allocate arrays and store repeated values                              \n\
  \  */                                                                                     \n\
  \                                                                                         \n\
  \%s                                                                                       \n\
  \                                                                                         \n\
  \  ptr = 32;                                                                              \n\
  \  while (ptr < data.length) {                                                            \n\
  \    (fieldId, wireType, bytesRead) = readKey(ptr, data);                                 \n\
  \    solType                        = fieldType(fieldId);                                 \n\
  \    ptr                           += bytesRead;                                          \n\
  \    counterId                      = repeatCounter(fieldId);                             \n\
  \                                                                                         \n\
  \    if (counterId >= 0) {                                                                \n\
  \      slot = counters[1][uint(counterId)];                                               \n\
  \                                                                                         \n\
  \      if(wireType == WireType.Varint) {                                                  \n\
  \        (resultUInt, bytesRead) = readVarInt(ptr, data);                                 \n\
  \        ptr += bytesRead;                                                                \n\
  \        if (solType == SolType.UInt) {                                                   \n\
  \          setArrayField_uint(slot, resultUInt, fieldId, buf);                            \n\
  \        } else if (solType == SolType.UInt32) {                                          \n\
  \          setArrayField_uint32(slot, uint32(resultUInt), fieldId, buf);                  \n\
  \        } else if (solType == SolType.UInt64) {                                          \n\
  \          setArrayField_uint64(slot, uint64(resultUInt), fieldId, buf);                  \n\
  \        } else {                                                                         \n\
  \          throw;                                                                         \n\
  \        }                                                                                \n\
  \      } else if(wireType == WireType.Fixed64) {                                          \n\
  \        resultUInt = uint(readUInt64(ptr, data));                                        \n\
  \        ptr += 8;                                                                        \n\
  \        setArrayField_uint64(slot, uint64(resultUInt), fieldId, buf);                    \n\
  \      } else if(wireType == WireType.LengthDelim) {                                      \n\
  \        (dataLen, bytesRead) = readVarInt(ptr, data);                                    \n\
  \        ptr += bytesRead;                                                                \n\
  \        if (solType == SolType.String) {                                                 \n\
  \          resultString = readString(ptr, data, dataLen);                                 \n\
  \          setArrayField_string(slot, resultString, fieldId, buf);                        \n\
  \        } else {                                                                         \n\
  \          throw;                                                                         \n\
  \        }                                                                                \n\
  \        ptr += dataLen;                                                                  \n\
  \      } else if(wireType == WireType.StartGroup) {                                       \n\
  \        throw; // Deprecated, not implemented                                            \n\
  \      } else if(wireType == WireType.EndGroup) {                                         \n\
  \        throw; // Deprecated, not implemented                                            \n\
  \      } else if(wireType == WireType.Fixed32) {                                          \n\
  \        resultUInt = uint(readUInt32(ptr, data));                                        \n\
  \        ptr += 4;                                                                        \n\
  \        setArrayField_uint32(slot, uint32(resultUInt), fieldId, buf);                    \n\
  \      }                                                                                  \n\
  \                                                                                         \n\
  \      counters[1][uint(counterId)] += 1;                                                 \n\
  \    } else {                                                                             \n\
  \      if(wireType == WireType.Varint) {                                                  \n\
  \        (resultUInt, bytesRead) = readVarInt(ptr, data);                                 \n\
  \        ptr += bytesRead;                                                                \n\
  \      } else if(wireType == WireType.Fixed64) {                                          \n\
  \        ptr += 8;                                                                        \n\
  \      } else if(wireType == WireType.LengthDelim) {                                      \n\
  \        (dataLen, bytesRead) = readVarInt(ptr, data);                                    \n\
  \        ptr += bytesRead;                                                                \n\
  \        ptr += dataLen;                                                                  \n\
  \      } else if(wireType == WireType.StartGroup) {                                       \n\
  \        throw; // Deprecated, not implemented                                            \n\
  \      } else if(wireType == WireType.EndGroup) {                                         \n\
  \        throw; // Deprecated, not implemented                                            \n\
  \      } else if(wireType == WireType.Fixed32) {                                          \n\
  \        ptr += 4;                                                                        \n\
  \      }                                                                                  \n\
  \    }                                                                                    \n\
  \ }                                                                                       \n\
  \                                                                                         \n\
  \  return buf;                                                                            \n\
  \}" name name name allocs

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

