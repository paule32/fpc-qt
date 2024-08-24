#!/bin/bash
pathdir="/E/Projekte/AthensProjekte/1"

echo "create linux fpcqt.so"
make
exit
echo "compile: server.cc"
cd $pathdir
cd server
qmake server.pro
make

echo "compile: fpc-qt.dll"
cd $pathdir
make
cd $(pwd)/Win64/Release
strip fpc-qt.dll
cp fpc-qt.dll ../Debug

