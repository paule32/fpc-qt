#!/bin/bash
# ---------------------------------------------------------------------------
# \file   build.sh
# \author Jens Kallup - paule32
# \date   (c) 2024 all rights reserved.
#
# \desc   Makefile for building fpc.dll and test data application.
# \bote   only for education, and non-profit usage.
#         commercial use is not allowed !
# ---------------------------------------------------------------------------
BASEDIR=$(cd "$(dirname "$0")" && pwd)

# ---------------------------------------------------------------------------
# 1. get the environment variables, based on the running system (RAD Delphi)
# ---------------------------------------------------------------------------
if [ -z "$1" ]; then
    echo "no parameter for the pascal compiler."
    read -n 1 -s
    exit
fi
if [ -z "$2" ]; then
    echo "no parameter for the used config."
    read -n 1 -s
    exit
fi
export PascalCompiler=$1
export RadConfig=$2

echo "create Windows fpcqt.dll"
cd $BASEDIR
make
echo "press any key to exit."
#read -n 1 -s
exit

echo "compile: server.cc"
cd $BASEDIR
cd server
qmake server.pro
make

echo "compile: fpc-qt.dll"
cd $pathdir
make
cd $(pwd)/Win64/Release
strip fpc-qt.dll
cp fpc-qt.dll ../Debug

