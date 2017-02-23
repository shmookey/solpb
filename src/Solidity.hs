module Solidity where

import qualified Data.Map as Map
import Text.Printf (printf)
import Data.List (intercalate, nub, partition)
import Data.Map (Map)

data Type = UInt | UInt32 | UInt64 | String | Bytes | Message StructName
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
    UInt      -> "uint"
    UInt32    -> "uint32"
    UInt64    -> "uint64"
    String    -> "string"
    Bytes     -> "bytes"
    Message m -> m ++ "Codec." ++ m

instance Format Setter where
  fmt (Setter structName setterType fields) =
    let
      typ     = fmt setterType
      safeTyp = map clean typ
      name    = "setField_" ++ safeTyp
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

clean :: Char -> Char
clean '.' = '_'
clean x   = x

formatLibrary :: Struct -> String
formatLibrary x@(Struct name fields) =
  let
    decoder  = indent 2 $ formatDecoder x (zip arrays [0..])
    statics  = indent 2 staticCode
    setters  = indent 2 $ createSetters name fields
    struct   = indent 2 $ fmt x
    ftypes   = indent 2 $ formatGetFieldType x
    counters = indent 2 $ formatGetRepeatCounter x counterIds

    (scalars,arrays) = partition isScalar fields
    counterIds       = (zip arrays [0..]) ++ (zip scalars $ repeat (-1))
  in
    printf "library %sCodec {\n%s\n%s\n%s\n%s\n%s\n%s\n}" name struct decoder counters ftypes statics setters

createSetters :: StructName -> [Field] -> String
createSetters name fields =
  let
    (scalars,arrays) = partition isScalar fields

    scalarTypes = nub $ [UInt, UInt32, UInt64, Bytes, String] ++ map msgType scalars
    arrayTypes  = nub $ [UInt, UInt32, UInt64, Bytes, String] ++ map msgType arrays
    scalarInit  = Map.fromList $ zip scalarTypes (repeat [])
    arrayInit   = Map.fromList $ zip arrayTypes (repeat [])

    accumulate :: Map Type [Field] -> Field -> Map Type [Field]
    accumulate acc x@(Field i k t _) = case Map.lookup t acc of
      Just xs -> Map.insert t (x:xs) acc
      Nothing -> Map.insert t [x] acc

    msgType :: Field -> Type
    msgType (Field _ _ t _) = t

    toSetters :: Map Type [Field] -> [Setter]
    toSetters = map (uncurry $ Setter name) . Map.toList

    scalarSetters = map fmt . toSetters $ foldl accumulate scalarInit scalars
    arraySetters  = map formatArraySetter . toSetters $ foldl accumulate arrayInit arrays
  in
    intercalate "\n" $ scalarSetters ++ arraySetters

isScalar :: Field -> Bool    
isScalar (Field _ _ _ x) = not (x == Repeated)

formatMessageDecoderScalar :: Struct -> String
formatMessageDecoderScalar (Struct name fields) =
  let
    isMsg (Field _ _ x l) = case (x,l) of (_, Repeated) -> False
                                          (Message _,_) -> True
                                          _             -> False
    msgs = filter isMsg fields
    ifs  = (intercalate " else " $ map iff msgs)

    iff :: Field -> String
    iff (Field i k t l) = printf "if (%s) {\n  %s\n}" cond action
      where cond   = (printf "fieldId == %i" i) :: String
            typ    = fmt t
            safeTyp = map clean typ
            action = (printf "setField_%s(%s.decode%s(ptr,data,dataLen), fieldId, buf);" typ typ typ) :: String
  
  in case msgs of
    [] -> ""
    _  -> " else " ++ ifs

formatMessageDecoderArray :: Struct -> String
formatMessageDecoderArray (Struct name fields) =
  let
    isMsg (Field _ _ x l) = 
      case (x,l) of (Message _, Repeated) -> True
                    _                     -> False
    msgs = filter isMsg fields
    ifs  = (intercalate " else " $ map iff msgs)

    iff :: Field -> String
    iff (Field i k t l) = printf "if (%s) {\n  %s\n}" cond action
      where cond   = (printf "fieldId == %i" i) :: String
            typ    = fmt t
            safeTyp = map clean typ
            
            action = (printf "setArrayField_%s(slot, %s.decode%s(ptr,data,dataLen), fieldId, buf);" 
                safeTyp (libName t) (msgTypeName t)) :: String
  in case msgs of
    [] -> ""
    _  -> " else " ++ ifs

libName :: Type -> String
libName (Message x) = x ++ "Codec"

msgTypeName :: Type -> String
msgTypeName (Message x) = x

formatGetRepeatCounter :: Struct -> [(Field, Int)] -> String
formatGetRepeatCounter (Struct name fields) counterIds =
  let
    proto = "function repeatCounter(uint fieldId) internal constant returns (int)"
    ifs   = (intercalate " else " $ map iff counterIds) ++ term
    term  = " else {\n  throw;\n}"

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
    safeTyp = map clean typ
    clean '.' = '_'
    clean x   = x
    name    = "setArrayField_" ++ safeTyp
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
            action = case t of 
                      Message _ -> "return SolType.Message;"
                      _         -> printf "return SolType.%s;" (solEnum t) :: String

  in
    printf "%s {\n%s}" proto (indent 2 ifs)

solEnum :: Type -> String
solEnum x = case x of
  UInt      -> "UInt"
  UInt32    -> "UInt32"
  UInt64    -> "UInt64"
  String    -> "String"
  Bytes     -> "Bytes"
  Message _ -> "Message"

formatDecoder :: Struct -> [(Field, Int)] -> String
formatDecoder x@(Struct name fields) counterIds = 
  let
    alloc :: (Field, Int) -> String
    alloc (Field _ k t _, i) = printf "buf.%s = new %s[](counters[0][%i]);" k (fmt t) i
    
    msgDecodeScalar = formatMessageDecoderScalar x
    msgDecodeArray  = formatMessageDecoderArray x
    numArrays       = length $ filter (\(_,i) -> i >= 0) counterIds

    allocs = indent 2 . intercalate "\n" $ map alloc counterIds
  in printf "\
  \function decode(bytes data) internal constant returns (%s) {                                 \n\
  \  return decode%s(32, data, data.length);                                                    \n\
  \}                                                                                            \n\
  \                                                                                             \n\
  \function decode%s(uint offset, bytes data, uint len) internal constant returns (%s) {        \n\
  \  %s memory buf;                                                                             \n\
  \  // The size of `data` and its first element are offset by a 256-bit length                 \n\
  \  uint[%i][2] memory counters;                                                               \n\
  \                                                                                             \n\
  \  // Declare these in advance to save stack space                                            \n\
  \  uint ptr;                                                                                  \n\
  \  uint bytesRead;                                                                            \n\
  \  int counterId;                                                                             \n\ 
  \  WireType wireType;                                                                         \n\
  \  SolType solType;                                                                           \n\
  \  uint fieldId;                                                                              \n\
  \                                                                                             \n\
  \  /* First pass: count instances of repeated values, read and store non-repeated values */   \n\
  \                                                                                             \n\
  \  ptr = offset;                                                                              \n\
  \  while (ptr < offset + len) {                                                               \n\
  \    (fieldId, wireType, bytesRead) = readKey(ptr, data);                                     \n\
  \    solType                        = fieldType(fieldId);                                     \n\
  \    ptr                           += bytesRead;                                              \n\
  \    counterId                      = repeatCounter(fieldId);                                 \n\
  \                                                                                             \n\
  \    if (counterId == -1) {                                                                   \n\
  \      // Non-repeated value. Read and store as usual.                                        \n\
  \      ptr += decodeScalar(ptr, data, fieldId, wireType, solType, buf);                       \n\
  \    } else {                                                                                 \n\
  \      // Repeated value. Update instance counter.                                            \n\
  \      // We still have to read most values but we don't yet have anywhere to store them.     \n\
  \      ptr += skipField(ptr, data, fieldId, wireType, solType);                               \n\
  \      counters[0][uint(counterId)] += 1;                                                     \n\
  \    }                                                                                        \n\
  \  }                                                                                          \n\
  \                                                                                             \n\
  \  /* Second pass: allocate arrays and store repeated values */                               \n\
  \                                                                                             \n\
  \%s                                                                                           \n\
  \                                                                                             \n\
  \  ptr = offset;                                                                              \n\
  \  while (ptr < offset + len) {                                                               \n\
  \    (fieldId, wireType, bytesRead) = readKey(ptr, data);                                     \n\
  \    solType                        = fieldType(fieldId);                                     \n\
  \    ptr                           += bytesRead;                                              \n\
  \    counterId                      = repeatCounter(fieldId);                                 \n\
  \                                                                                             \n\
  \    if (counterId >= 0) {                                                                    \n\
  \      ptr += decodeArrayElem(counters[1][uint(counterId)], ptr, data, fieldId,               \n\
  \        wireType, solType, buf);                                                             \n\
  \      counters[1][uint(counterId)] += 1;                                                     \n\
  \    } else {                                                                                 \n\
  \      ptr += skipField(ptr, data, fieldId, wireType, solType);                               \n\
  \    }                                                                                        \n\
  \  }                                                                                          \n\
  \  return buf;                                                                                \n\
  \}                                                                                            \n\
  \                                                                                             \n\
  \function skipField(uint ptr, bytes data, uint fieldId, WireType wireType,                    \n\
  \    SolType solType) internal constant returns (uint) {                                      \n\
  \  uint bytesRead;                                                                            \n\
  \  uint dataLen;                                                                              \n\
  \  if(wireType == WireType.Varint) {                                                          \n\
  \    (, bytesRead) = readVarInt(ptr, data);                                                   \n\
  \  } else if(wireType == WireType.Fixed64) {                                                  \n\
  \    bytesRead = 8;                                                                           \n\
  \  } else if(wireType == WireType.LengthDelim) {                                              \n\
  \    (dataLen, bytesRead) = readVarInt(ptr, data);                                            \n\
  \    bytesRead += dataLen;                                                                    \n\
  \  } else if(wireType == WireType.StartGroup) {                                               \n\
  \    throw; // Deprecated, not implemented                                                    \n\
  \  } else if(wireType == WireType.EndGroup) {                                                 \n\
  \    throw; // Deprecated, not implemented                                                    \n\
  \  } else if(wireType == WireType.Fixed32) {                                                  \n\
  \    bytesRead = 4;                                                                           \n\
  \  } else {                                                                                   \n\
  \    throw;                                                                                   \n\
  \  }                                                                                          \n\
  \  return bytesRead;                                                                          \n\
  \}                                                                                            \n\
  \                                                                                             \n\
  \function decodeScalar(uint ptr, bytes data, uint fieldId, WireType wireType,                 \n\
  \    SolType solType, %s buf) internal constant returns (uint) {                              \n\
  \  uint bytesRead;                                                                            \n\
  \  uint dataLen;                                                                              \n\
  \  if(wireType == WireType.Varint) {                                                          \n\
  \    uint resultUInt;                                                                         \n\
  \    (resultUInt, bytesRead) = readVarInt(ptr, data);                                         \n\
  \    if (solType == SolType.UInt) {                                                           \n\
  \      setField_uint(resultUInt, fieldId, buf);                                               \n\
  \    } else if (solType == SolType.UInt32) {                                                  \n\
  \      setField_uint32(uint32(resultUInt), fieldId, buf);                                     \n\
  \    } else if (solType == SolType.UInt64) {                                                  \n\
  \      setField_uint64(uint64(resultUInt), fieldId, buf);                                     \n\
  \    } else {                                                                                 \n\
  \      throw;                                                                                 \n\
  \    }                                                                                        \n\
  \  } else if(wireType == WireType.Fixed64) {                                                  \n\
  \    uint64 resultUInt64 = readUInt64(ptr, data);                                             \n\
  \    setField_uint64(resultUInt64, fieldId, buf);                                             \n\
  \    bytesRead = 8;                                                                           \n\
  \  } else if(wireType == WireType.LengthDelim) {                                              \n\
  \    (dataLen, bytesRead) = readVarInt(ptr, data);                                            \n\
  \    ptr += bytesRead;                                                                        \n\
  \    if (solType == SolType.String) {                                                         \n\
  \      string memory resultString = readString(ptr, data, dataLen);                           \n\
  \      setField_string(resultString, fieldId, buf);                                           \n\
  \    } else if (solType == SolType.Bytes) {                                                   \n\
  \      bytes memory resultBytes = readBytes(ptr, data, dataLen);                              \n\
  \      setField_bytes(resultBytes, fieldId, buf);                                             \n\
  \    } %s else {                                                                              \n\
  \      throw;                                                                                 \n\
  \    }                                                                                        \n\
  \    bytesRead += dataLen;                                                                    \n\
  \  } else if(wireType == WireType.StartGroup) {                                               \n\
  \    throw; // Deprecated, not implemented                                                    \n\
  \  } else if(wireType == WireType.EndGroup) {                                                 \n\
  \    throw; // Deprecated, not implemented                                                    \n\
  \  } else if(wireType == WireType.Fixed32) {                                                  \n\
  \    resultUInt = uint(readUInt32(ptr, data));                                                \n\
  \    bytesRead = 4;                                                                           \n\
  \    setField_uint32(uint32(resultUInt), fieldId, buf);                                       \n\
  \  } else {                                                                                   \n\
  \    throw;                                                                                   \n\
  \  }                                                                                          \n\
  \  return bytesRead;                                                                          \n\
  \}                                                                                            \n\
  \                                                                                             \n\
  \function decodeArrayElem(uint slot, uint ptr, bytes data, uint fieldId, WireType wireType,   \n\
  \    SolType solType, %s buf) internal constant returns (uint) {                              \n\
  \  uint bytesRead;                                                                            \n\
  \  if(wireType == WireType.Varint) {                                                          \n\
  \    uint resultUInt;                                                                         \n\
  \    (resultUInt, bytesRead) = readVarInt(ptr, data);                                         \n\
  \    if (solType == SolType.UInt) {                                                           \n\
  \      setArrayField_uint(slot, resultUInt, fieldId, buf);                                    \n\
  \    } else if (solType == SolType.UInt32) {                                                  \n\
  \      setArrayField_uint32(slot, uint32(resultUInt), fieldId, buf);                          \n\
  \    } else if (solType == SolType.UInt64) {                                                  \n\
  \      setArrayField_uint64(slot, uint64(resultUInt), fieldId, buf);                          \n\
  \    } else {                                                                                 \n\
  \      throw;                                                                                 \n\
  \    }                                                                                        \n\
  \  } else if(wireType == WireType.Fixed64) {                                                  \n\
  \    uint64 resultUInt64 = readUInt64(ptr, data);                                             \n\
  \    bytesRead = 8;                                                                           \n\
  \    setArrayField_uint64(slot, resultUInt64, fieldId, buf);                                  \n\
  \  } else if(wireType == WireType.LengthDelim) {                                              \n\
  \    uint dataLen;                                                                            \n\
  \    (dataLen, bytesRead) = readVarInt(ptr, data);                                            \n\
  \    ptr += bytesRead;                                                                        \n\
  \    bytesRead += dataLen;                                                                    \n\
  \    if (solType == SolType.String) {                                                         \n\
  \      string memory resultString = readString(ptr, data, dataLen);                           \n\
  \      setArrayField_string(slot, resultString, fieldId, buf);                                \n\
  \    } else if (solType == SolType.Bytes) {                                                   \n\
  \      bytes memory resultBytes = readBytes(ptr, data, dataLen);                              \n\
  \      setArrayField_bytes(slot, resultBytes, fieldId, buf);                                  \n\
  \    } %s else {                                                                              \n\
  \      throw;                                                                                 \n\
  \    }                                                                                        \n\
  \  } else if(wireType == WireType.StartGroup) {                                               \n\
  \    throw; // Deprecated, not implemented                                                    \n\
  \  } else if(wireType == WireType.EndGroup) {                                                 \n\
  \    throw; // Deprecated, not implemented                                                    \n\
  \  } else if(wireType == WireType.Fixed32) {                                                  \n\
  \    uint32 resultUInt32 = readUInt32(ptr, data);                                             \n\
  \    bytesRead = 4;                                                                           \n\
  \    setArrayField_uint32(slot, resultUInt32, fieldId, buf);                                  \n\
  \  } else {                                                                                   \n\
  \    throw;                                                                                   \n\
  \  }                                                                                          \n\
  \  return bytesRead;                                                                          \n\
  \}" name name name name name numArrays allocs name msgDecodeScalar name msgDecodeArray

staticCode :: String
staticCode = "\
  \enum WireType { Varint, Fixed64, LengthDelim, StartGroup, EndGroup, Fixed32 }                \n\
  \enum SolType { UInt, UInt32, UInt64, String, Bytes, Message }                                \n\
  \                                                                                             \n\
  \function readKey(uint ptr, bytes data) constant returns (uint, WireType, uint) {             \n\
  \  var (x, n) = readVarInt(ptr, data);                                                        \n\
  \  WireType typeId  = WireType(x & 7);                                                        \n\
  \  uint fieldId = x / 8; //x >> 3;                                                            \n\
  \  return (fieldId, typeId, n);                                                               \n\
  \}                                                                                            \n\
  \                                                                                             \n\
  \function readBytes(uint ptr, bytes data, uint length) constant returns (bytes) {             \n\
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
  \  return b;                                                                                  \n\
  \}                                                                                            \n\
  \                                                                                             \n\
  \function readString(uint ptr, bytes data, uint length) constant returns (string) {           \n\
  \  return string(readBytes(ptr, data, length));                                               \n\
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
  \                                                                                             \n\
  \function readSInt(bytes input, uint ptr) returns (int signedInt, uint) {                     \n\
  \  var (varInt, bytesUsed) = readVarInt(ptr, input);                                          \n\
  \  assembly {                                                                                 \n\
  \    signedInt := xor(div(varInt, 2), add(not(and(varInt, 1)), 1))                            \n\
  \  }                                                                                          \n\
  \  return (signedInt, bytesUsed);                                                             \n\
  \}                                                                                            \n\
  \"

