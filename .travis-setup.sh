#!/bin/bash

set -e 

DEPS_URL=https://s3-ap-southeast-2.amazonaws.com/protobuf-solidity/build-deps

mkdir -p $HOME/.local/bin

if [[ "$TRAVIS_OS_NAME" == "osx" ]]
then
  curl --insecure -L https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C $HOME/.local/bin
  brew tap ethereum/ethereum
  brew install solidity
  brew linkapps solidity
fi

if [[ "$TRAVIS_OS_NAME" == "linux" ]] 
then
  gem install --no-ri --no-rdoc fpm
  curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C $HOME/.local/bin '*/stack'
  sudo add-apt-repository -y ppa:ethereum/ethereum
  sudo apt-get update
  sudo apt-get install -y solc
fi

curl $DEPS_URL/$TRAVIS_OS_NAME/evm -o $HOME/.local/bin/evm
chmod +x $HOME/.local/bin/evm

