# This makefile is meant for use with Travis CI, but should work for local builds.

TRAVIS_OS_NAME ?= $(shell uname)
TRAVIS_BRANCH ?= $(shell git rev-parse --abbrev-ref HEAD)

OS = $(TRAVIS_OS_NAME)
BRANCH = $(TRAVIS_BRANCH)
VERSION = $(shell cat VERSION)
LOCAL_BIN = $(shell stack path --local-bin)

ifeq ($(BRANCH),master)
BUILD = $(VERSION)
else
BUILD = $(VERSION)-$(BRANCH)
endif


solpb:
	stack --copy-bins --no-terminal build
	mkdir -p build/binaries/$(BUILD)
	cp $(LOCAL_BIN)/solpb build/binaries/$(BUILD)/solpb-$(BUILD)-$(OS)

pkg: solpb
	mkdir -p tmp/solpb-$(BUILD)/bin
	mkdir -p build/packages/tarball
	cp $(LOCAL_BIN)/solpb tmp/solpb-$(BUILD)/bin/solpb
	cp pkg-install.sh tmp/solpb-$(BUILD)/install.sh
	cp LICENSE tmp/solpb-$(BUILD)/LICENCE
	echo $(BUILD) > tmp/solpb-$(BUILD)/VERSION
	tar -cf build/packages/tarball/solpb-$(BUILD)-$(OS).tar.gz -C tmp solpb-$(BUILD)
	
clean:
	rm -rf build
	rm -rf tmp
	
