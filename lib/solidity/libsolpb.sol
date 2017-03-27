pragma solidity ^0.4.0;

library _pb {

    enum WireType { Varint, Fixed64, LengthDelim, StartGroup, EndGroup, Fixed32 }

    // Decoders

    function _decode_uint32(uint p, bytes bs, WireType wt)
        internal constant returns (uint32, uint) {
      if(wt == WireType.Varint) {
        var (varint, sz) = _decode_varint(p, bs);
        return (uint32(varint), sz);
      } else if (wt == WireType.Fixed32) {
        return _decode_uint32f(p, bs);
      } else throw;
    }

    function _decode_uint64(uint p, bytes bs, WireType wt)
        internal constant returns (uint64, uint) {
      if(wt == WireType.Varint) {
        var (varint, sz) = _decode_varint(p, bs);
        return (uint64(varint), sz);
      } else if (wt == WireType.Fixed64) {
        return _decode_uint64f(p, bs);
      } else throw;
    }

    function _decode_int32(uint p, bytes bs, WireType wt)
        internal constant returns (int32, uint) {
      if(wt == WireType.Varint) {
        var (varint, sz) = _decode_varints(p, bs);
        return (int32(varint), sz);
      } else if (wt == WireType.Fixed32) {
        return _decode_int32f(p, bs);
      } else throw;
    }

    function _decode_int64(uint p, bytes bs, WireType wt)
        internal constant returns (int64, uint) {
      if(wt == WireType.Varint) {
        var (varint, sz) = _decode_varints(p, bs);
        return (int64(varint), sz);
      } else if (wt == WireType.Fixed64) {
        return _decode_int64f(p, bs);
      } else throw;
    }

    function _decode_string(uint p, bytes bs, WireType)
        internal constant returns (string, uint) {
      var (x, sz) = _decode_lendelim(p, bs);
      return (string(x), sz);
    }

    function _decode_bytes(uint p, bytes bs, WireType)
        internal constant returns (bytes, uint) {
      return _decode_lendelim(p, bs);
    }

    function _decode_key(uint p, bytes bs)
        internal constant returns (uint, WireType, uint) {
      var (x, n) = _decode_varint(p, bs);
      WireType typeId  = WireType(x & 7);
      uint fieldId = x / 8; //x >> 3;
      return (fieldId, typeId, n);
    }

    function _decode_varint(uint p, bytes bs)
        internal constant returns (uint, uint) {
      uint x = 0;
      uint sz = 0;
      assembly {
        let b := 0
        p     := add(bs, p)
        loop:
          b  := byte(0, mload(p))
          x  := or(x, mul(and(0x7f, b), exp(2, mul(7, sz))))
          sz := add(sz, 1)
          p  := add(p, 0x01)
          jumpi(loop, eq(0x80, and(b, 0x80)))
      }
      return (x, sz);
    }

    function _decode_varints(uint p, bytes bs)
        internal constant returns (int, uint) {
      var (u, sz) = _decode_varint(p, bs);
      int s;
      assembly {
        s := xor(div(u, 2), add(not(and(u, 1)), 1))
      }
      return (s, sz);
    }

    function _decode_uintf(uint p, bytes bs, uint sz)
        internal constant returns (uint, uint) {
      uint x = 0;
      assembly {
        let i := 0
        p     := add(bs, p)
        loop:
          jumpi(end, eq(i, sz))
          x := or(x, mul(byte(0, mload(p)), exp(2, mul(8, i))))
          p := add(p, 0x01)
          i := add(i, 1)
          jump(loop)
        end:
      }
      return (x, sz);
    }

    function _decode_uint32f(uint p, bytes bs)
        internal constant returns (uint32, uint) {
      var (x, sz) = _decode_uintf(p, bs, 4);
      return (uint32(x), sz);
    }

    function _decode_uint64f(uint p, bytes bs)
        internal constant returns (uint64, uint) {
      var (x, sz) = _decode_uintf(p, bs, 8);
      return (uint64(x), sz);
    }

    function _decode_int32f(uint p, bytes bs)
        internal constant returns (int32, uint) {
      var (x, sz) = _decode_uintf(p, bs, 4);
      int r; assembly { r := x }
      return (int32(r), sz);
    }

    function _decode_int64f(uint p, bytes bs)
        internal constant returns (int64, uint) {
      var (x, sz) = _decode_uintf(p, bs, 8);
      int r; assembly { r := x }
      return (int64(r), sz);
    }

    function _decode_lendelim(uint p, bytes bs)
        internal constant returns (bytes, uint) {
      var (len, sz) = _decode_varint(p, bs);
      bytes memory b = new bytes(len);
      assembly {
        let bptr  := add(b, 32)
        let count := 0
        p         := add(add(bs, p),sz)
        loop :
          jumpi(end, eq(count, len))
          mstore8(bptr, byte(0, mload(p)))
          p     := add(p, 1)
          bptr  := add(bptr, 1)
          count := add(count, 1)
          jump(loop)
        end:
      }
      return (b, sz+len);
    }

  // Encoders

  function _encode_key(uint x, WireType wt, uint p, bytes bs)
      internal constant returns (uint) {
    uint i;
    assembly {
      i := or(mul(x, 8), mod(wt, 8))
    }
    return _encode_varint(i, p, bs);
  }
  function _encode_varint(uint x, uint p, bytes bs)
      internal constant returns (uint) {
    uint sz = 0;
    assembly {
      let bsptr := add(bs, p)
      let byt := 0
      let pbyt := 0
      loop:
        byt := and(div(x, exp(2, mul(7, sz))), 0x7f)
        pbyt := and(div(x, exp(2, mul(7, add(sz, 1)))), 0x7f)
        jumpi(end, eq(pbyt, 0))
        mstore8(bsptr, or(0x80, byt))
        bsptr := add(bsptr, 1)
        sz := add(sz, 1)
        jump(loop)
      end:
        byt := and(div(x, exp(2, mul(7, sz))), 0x7f)
        mstore8(bsptr, byt)
        sz := add(sz, 1)
    }
    return sz;
  }
  function _encode_varints(int x, uint p, bytes bs)
      internal constant returns (uint) {
    uint encodedInt = zigZagEncode(x);
    return _encode_varint(encodedInt, p, bs);
  }
  function _encode_bytes(bytes xs, uint p, bytes bs)
      internal constant returns (uint) {
    uint xsLength = xs.length;
    uint sz = _encode_varint(xsLength, p, bs);
    uint count = 0;
    assembly {
      let bsptr := add(bs, add(p, sz))
      let xsptr := add(xs, 32)
      loop:
        jumpi(end, eq(count, xsLength))
        mstore8(bsptr, byte(0, mload(xsptr)))
        bsptr := add(bsptr, 1)
        xsptr := add(xsptr, 1)
        count := add(count, 1)
        jump(loop)
      end:
    }
    return sz+count;
  }
  function _encode_string(string xs, uint p, bytes bs)
      internal constant returns (uint) {
    return  _encode_bytes(bytes(xs), p, bs);
  }
  function _encode_uint32f(uint32 x, uint p, bytes bs)
      internal constant returns (uint) {
    return _encode_uintf(x, p, bs, 4);
  }
  function _encode_uint64f(uint64 x, uint p, bytes bs)
      internal constant returns (uint) {
    return _encode_uintf(x, p, bs, 8);
  }
  function _encode_int32f(int32 x, uint p, bytes bs)
      internal constant returns (uint) {
    uint encodedInt = zigZagEncode(x);
    return _encode_uintf(encodedInt, p, bs, 4);
  }
  function _encode_int64f(int64 x, uint p, bytes bs)
      internal constant returns (uint) {
    uint encodedInt = zigZagEncode(x);
    return _encode_uintf(encodedInt, p, bs, 8);
  }
  function _encode_uintf(uint x, uint p, bytes bs, uint sz)
    internal constant returns (uint) {
    assembly {
      let bsptr := add(bs, p)
      let count := sz
      loop:
        jumpi(end, eq(count, 0))
        mstore8(bsptr, byte(sub(32, count), x))
        bsptr := add(bsptr, 1)
        count := sub(count, 1)
        jump(loop)
      end:
    }
    return sz;
  }
  function zigZagEncode(int i) internal returns (uint) {
    uint encodedInt;
    int x = -1;
    assembly {
      let mask := 0
      jumpi(next, sgt(i, 0))
      mask := x
      next:
        encodedInt := xor(mul(i, 2), mask)
    }
    return encodedInt;
  }

  // Estimators

  function _sz_lendelim(uint i) internal constant returns (uint) {
    return i + _sz_varint(i);
  }

  function _sz_key(uint i) internal constant returns (uint) {
    if(i < 16) return 1;
    else if(i < 2048) return 2;
    else if(i < 262144) return 3;
    else throw;
  }

  function _sz_varint(uint i) internal constant returns (uint) {
    if(i < 128) return 1;
    else if(i < 16384) return 2;
    else if(i < 2097152) return 3;
    else if(i < 268435456) return 4;
    else throw;
  }

  // Soltype extensions

  function _decode_sol_bytesN(uint8 n, uint p, bytes bs)
      internal constant returns (bytes32, uint) {
    uint r;
    var (len, sz) = _decode_varint(p, bs);
    if (len + sz != n + 3) throw;
    p += 3;
    assembly { r := mload(add(p,bs)) }
    for (uint i=n; i<32; i++)
      r /= 256;
    return (bytes32(r), n + 3);
  }

  function _decode_sol_address(uint p, bytes bs, WireType)
      internal constant returns (address, uint) {
    var (r, sz) = _decode_sol_bytesN(20, p, bs);
    return (address(r), sz);
  }

  function _decode_sol_uint(uint p, bytes bs, WireType wt)
      internal constant returns (uint, uint) {
    return _decode_sol_uint256(p, bs, wt);
  }

  function _decode_sol_uintN(uint8 n, uint p, bytes bs)
      internal constant returns (uint, uint) {
    var (u, sz) = _decode_sol_bytesN(n, p, bs);
    uint r; assembly { r := u }
    return (r, sz);
  }

  function _decode_sol_uint8(uint p, bytes bs, WireType)
      internal constant returns (uint8, uint) {
    var (r, sz) = _decode_sol_uintN(1, p, bs);
    return (uint8(r), sz);
  }

  function _decode_sol_uint16(uint p, bytes bs, WireType)
      internal constant returns (uint16, uint) {
    var (r, sz) = _decode_sol_uintN(2, p, bs);
    return (uint16(r), sz);
  }

  function _decode_sol_uint32(uint p, bytes bs, WireType)
      internal constant returns (uint32, uint) {
    var (r, sz) = _decode_sol_uintN(4, p, bs);
    return (uint32(r), sz);
  }

  function _decode_sol_uint64(uint p, bytes bs, WireType)
      internal constant returns (uint64, uint) {
    var (r, sz) = _decode_sol_uintN(8, p, bs);
    return (uint64(r), sz);
  }

  function _decode_sol_uint128(uint p, bytes bs, WireType)
      internal constant returns (uint128, uint) {
    var (r, sz) = _decode_sol_uintN(16, p, bs);
    return (uint128(r), sz);
  }

  function _decode_sol_uint256(uint p, bytes bs, WireType)
      internal constant returns (uint256, uint) {
    var (r, sz) = _decode_sol_uintN(32, p, bs);
    return (uint256(r), sz);
  }

  function _decode_sol_int(uint p, bytes bs, WireType wt)
      internal constant returns (int, uint) {
    return _decode_sol_int256(p, bs, wt);
  }

  function _decode_sol_intN(uint8 n, uint p, bytes bs)
      internal constant returns (int, uint) {
    var (u, sz) = _decode_sol_bytesN(n, p, bs);
    int r; assembly { r := u }
    return (r, sz);
  }

  function _decode_sol_int8(uint p, bytes bs, WireType)
      internal constant returns (int8, uint) {
    var (r, sz) = _decode_sol_intN(1, p, bs);
    return (int8(r), sz);
  }

  function _decode_sol_int16(uint p, bytes bs, WireType)
      internal constant returns (int16, uint) {
    var (r, sz) = _decode_sol_intN(2, p, bs);
    return (int16(r), sz);
  }

  function _decode_sol_int32(uint p, bytes bs, WireType)
      internal constant returns (int32, uint) {
    var (r, sz) = _decode_sol_intN(4, p, bs);
    return (int32(r), sz);
  }

  function _decode_sol_int64(uint p, bytes bs, WireType)
      internal constant returns (int64, uint) {
    var (r, sz) = _decode_sol_intN(8, p, bs);
    return (int64(r), sz);
  }

  function _decode_sol_int128(uint p, bytes bs, WireType)
      internal constant returns (int128, uint) {
    var (r, sz) = _decode_sol_intN(16, p, bs);
    return (int128(r), sz);
  }

  function _decode_sol_int256(uint p, bytes bs, WireType)
      internal constant returns (int256, uint) {
    var (r, sz) = _decode_sol_intN(32, p, bs);
    return (int256(r), sz);
  }

  function _decode_sol_bytes1(uint p, bytes bs, WireType)
      internal constant returns (bytes1, uint) {
    var (r, sz) = _decode_sol_bytesN(1, p, bs);
    return (bytes1(r), sz);
  }

  function _decode_sol_bytes2(uint p, bytes bs, WireType)
      internal constant returns (bytes2, uint) {
    var (r, sz) = _decode_sol_bytesN(2, p, bs);
    return (bytes2(r), sz);
  }

  function _decode_sol_bytes3(uint p, bytes bs, WireType)
      internal constant returns (bytes3, uint) {
    var (r, sz) = _decode_sol_bytesN(3, p, bs);
    return (bytes3(r), sz);
  }

  function _decode_sol_bytes4(uint p, bytes bs, WireType)
      internal constant returns (bytes4, uint) {
    var (r, sz) = _decode_sol_bytesN(4, p, bs);
    return (bytes4(r), sz);
  }

  function _decode_sol_bytes5(uint p, bytes bs, WireType)
      internal constant returns (bytes5, uint) {
    var (r, sz) = _decode_sol_bytesN(5, p, bs);
    return (bytes5(5), sz);
  }

  function _decode_sol_bytes6(uint p, bytes bs, WireType)
      internal constant returns (bytes6, uint) {
    var (r, sz) = _decode_sol_bytesN(6, p, bs);
    return (bytes6(r), sz);
  }

  function _decode_sol_bytes7(uint p, bytes bs, WireType)
      internal constant returns (bytes7, uint) {
    var (r, sz) = _decode_sol_bytesN(7, p, bs);
    return (bytes7(r), sz);
  }

  function _decode_sol_bytes8(uint p, bytes bs, WireType)
      internal constant returns (bytes8, uint) {
    var (r, sz) = _decode_sol_bytesN(8, p, bs);
    return (bytes8(r), sz);
  }

  function _decode_sol_bytes9(uint p, bytes bs, WireType)
      internal constant returns (bytes9, uint) {
    var (r, sz) = _decode_sol_bytesN(9, p, bs);
    return (bytes9(r), sz);
  }

  function _decode_sol_bytes10(uint p, bytes bs, WireType)
      internal constant returns (bytes10, uint) {
    var (r, sz) = _decode_sol_bytesN(10, p, bs);
    return (bytes10(r), sz);
  }

  function _decode_sol_bytes11(uint p, bytes bs, WireType)
      internal constant returns (bytes11, uint) {
    var (r, sz) = _decode_sol_bytesN(11, p, bs);
    return (bytes11(r), sz);
  }

  function _decode_sol_bytes12(uint p, bytes bs, WireType)
      internal constant returns (bytes12, uint) {
    var (r, sz) = _decode_sol_bytesN(12, p, bs);
    return (bytes12(r), sz);
  }

  function _decode_sol_bytes13(uint p, bytes bs, WireType)
      internal constant returns (bytes13, uint) {
    var (r, sz) = _decode_sol_bytesN(13, p, bs);
    return (bytes13(r), sz);
  }

  function _decode_sol_bytes14(uint p, bytes bs, WireType)
      internal constant returns (bytes14, uint) {
    var (r, sz) = _decode_sol_bytesN(14, p, bs);
    return (bytes14(r), sz);
  }

  function _decode_sol_bytes15(uint p, bytes bs, WireType)
      internal constant returns (bytes15, uint) {
    var (r, sz) = _decode_sol_bytesN(15, p, bs);
    return (bytes15(15), sz);
  }

  function _decode_sol_bytes16(uint p, bytes bs, WireType)
      internal constant returns (bytes16, uint) {
    var (r, sz) = _decode_sol_bytesN(16, p, bs);
    return (bytes16(r), sz);
  }

  function _decode_sol_bytes17(uint p, bytes bs, WireType)
      internal constant returns (bytes17, uint) {
    var (r, sz) = _decode_sol_bytesN(17, p, bs);
    return (bytes17(r), sz);
  }

  function _decode_sol_bytes18(uint p, bytes bs, WireType)
      internal constant returns (bytes18, uint) {
    var (r, sz) = _decode_sol_bytesN(18, p, bs);
    return (bytes18(r), sz);
  }

  function _decode_sol_bytes19(uint p, bytes bs, WireType)
      internal constant returns (bytes19, uint) {
    var (r, sz) = _decode_sol_bytesN(19, p, bs);
    return (bytes19(r), sz);
  }

  function _decode_sol_bytes20(uint p, bytes bs, WireType)
      internal constant returns (bytes20, uint) {
    var (r, sz) = _decode_sol_bytesN(20, p, bs);
    return (bytes20(r), sz);
  }

  function _decode_sol_bytes21(uint p, bytes bs, WireType)
      internal constant returns (bytes21, uint) {
    var (r, sz) = _decode_sol_bytesN(21, p, bs);
    return (bytes21(r), sz);
  }

  function _decode_sol_bytes22(uint p, bytes bs, WireType)
      internal constant returns (bytes22, uint) {
    var (r, sz) = _decode_sol_bytesN(22, p, bs);
    return (bytes22(r), sz);
  }

  function _decode_sol_bytes23(uint p, bytes bs, WireType)
      internal constant returns (bytes23, uint) {
    var (r, sz) = _decode_sol_bytesN(23, p, bs);
    return (bytes23(r), sz);
  }

  function _decode_sol_bytes24(uint p, bytes bs, WireType)
      internal constant returns (bytes24, uint) {
    var (r, sz) = _decode_sol_bytesN(24, p, bs);
    return (bytes24(r), sz);
  }

  function _decode_sol_bytes25(uint p, bytes bs, WireType)
      internal constant returns (bytes25, uint) {
    var (r, sz) = _decode_sol_bytesN(25, p, bs);
    return (bytes25(15), sz);
  }

  function _decode_sol_bytes26(uint p, bytes bs, WireType)
      internal constant returns (bytes26, uint) {
    var (r, sz) = _decode_sol_bytesN(26, p, bs);
    return (bytes26(r), sz);
  }

  function _decode_sol_bytes27(uint p, bytes bs, WireType)
      internal constant returns (bytes27, uint) {
    var (r, sz) = _decode_sol_bytesN(27, p, bs);
    return (bytes27(r), sz);
  }

  function _decode_sol_bytes28(uint p, bytes bs, WireType)
      internal constant returns (bytes28, uint) {
    var (r, sz) = _decode_sol_bytesN(28, p, bs);
    return (bytes28(r), sz);
  }

  function _decode_sol_bytes29(uint p, bytes bs, WireType)
      internal constant returns (bytes29, uint) {
    var (r, sz) = _decode_sol_bytesN(29, p, bs);
    return (bytes29(r), sz);
  }

  function _decode_sol_bytes30(uint p, bytes bs, WireType)
      internal constant returns (bytes30, uint) {
    var (r, sz) = _decode_sol_bytesN(30, p, bs);
    return (bytes30(r), sz);
  }

  function _decode_sol_bytes31(uint p, bytes bs, WireType)
      internal constant returns (bytes31, uint) {
    var (r, sz) = _decode_sol_bytesN(31, p, bs);
    return (bytes31(r), sz);
  }

  function _decode_sol_bytes32(uint p, bytes bs, WireType)
      internal constant returns (bytes32, uint) {
    return _decode_sol_bytesN(32, p, bs);
  }

}
