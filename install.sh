#!/bin/bash

export MOSYNCDIR=~/mosync
BIN=$MOSYNCDIR/bin

echo $BIN

mkdir -p $BIN

cp build/gcc/gcc/xgcc $BIN
cp build/gcc/gcc/cpp $BIN
cp build/gcc/gcc/cc1 $BIN
cp build/gcc/gcc/cc1plus $BIN

echo installation complete.

