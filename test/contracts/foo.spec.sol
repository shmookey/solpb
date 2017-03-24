library FooSpec {
  function test() returns (uint) {
    uint8[4] memory data = [0x08, 0x45, 0x10, 0x2a];
    bytes memory bs = new bytes(4);
    for(uint i=0; i<4; i++) bs[i] = bytes1(data[i]);
    FooCodec.Foo memory r = FooCodec.decode(bs);
    return r.foo;
  }
  function decode(bytes bs) returns (uint) {
    FooCodec.Foo memory r = FooCodec.decode(bs);
    return r.foo;
  }
}

