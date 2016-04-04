#!/bin/sh

# To build the binary - likely to be called from Makefile inside a docker container
make cabal-init
cabal test
cabal build
cp dist/build/pacemaker/pacemaker $PWD/pacemaker
