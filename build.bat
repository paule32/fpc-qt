@echo off
set MSYS2_PATH=E:\msys64.new
set MSYSTEM=MINGW64
set PATH=%MSYS2_PATH%\usr\bin;%MSYS2_PATH%\mingw64\bin;%PATH%

set PascalCompiler=%1
set RadConfig=%2

%MSYS2_PATH%\usr\bin\bash -lc "/E/Projekte/AthensProjekte/1/build.sh Delphi Release"
::pause
::exit
