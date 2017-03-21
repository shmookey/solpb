# Protocol buffers for Solidity

[![Build Status](https://travis-ci.com/CBAInnovationLab/protobuf-solidity.svg?token=s2ifT26XSGNFYpJF3fSf&branch=master)](https://travis-ci.com/CBAInnovationLab/protobuf-solidity)

This project aims to provide an implementation of Google's protocol buffers for
smart contracts written in Solidity, targeting Ethereum-based blockchains.

In effect, this tool extends Solidity to allow the use of structs as arguments
and return values for external functions.


## Summary

The protocol buffers specification consists of a simple schema notation for
messages containing structured data, and a compact binary encoding scheme for
efficient and language-neutral transmission of message data.

In a typical use case, message schemas (usually in the form of `.proto` files)
are compiled by a build-time tool in order to generate code (e.g. class files)
in the target language, implementing the message format with native data types
and structures and providing methods for serialising and deserialising binary-
formatted messages. Many implementations involve a run-time library component
in addition to the generated code. 

`solpb` is a command-line program for generating Solidity code from `.proto`
files. One "codec" is generated per message type as separate libraries, each
of which contains the message type definition as a struct, and a `decode`
function which takes a `bytes` array and returns the decoded struct.

So far, most of version 2.0 of the specification is in a working state.


## Install

Mac users can install the latest stable version of the solpb via homebrew:

```bash
brew install CBAInnovationLab/tap/solpb
```

Debian-based linux distributions such as Ubuntu can use the deb package:

```bash
wget https://s3-ap-southeast-2.amazonaws.com/protobuf-solidity/packages/debian/solpb-0.1.0-linux-x86_64.deb
dpkg -i solpb-0.1.0-linux-x86_64.deb
```

Other users may install from the latest binary tarball:

```bash
tar xvf solpb-0.1.0-linux.tar.gz
cd solpb-0.1.0
./install.sh
```

Alternatively, you may always build from source (requires Stack):

```bash
git clone https://github.com/CBAInnovationLab/protobuf-solidity.git
cd protobuf-solidity
stack setup
stack build
```

