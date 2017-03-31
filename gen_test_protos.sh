#!/bin/bash

stack exec hprotoc -- -I lib/proto -I test/proto -d test -p Gen Prim.proto Arrays.proto SolTypes.proto Structs.proto Simple.proto solidity.proto

