#!/bin/bash

cd /E/Projekte/AthensProjekte/1
make
cd $(pwd)/Win64/Release
strip fpc-qt.dll
