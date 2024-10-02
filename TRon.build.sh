#!/bin/bash

# This script is provided by:  TRon  from the Lazarus-Forum:
# https://forum.lazarus.freepascal.org/index.php
#
# dependencies:
# - bash
# - sed
# - wget
# - unzip
# - fpc
# - g++
# - QT5
# - 3.5MB of diskspace
# - internet connection or local stored zip-archive of repository with correct hash


configure()
{
  Qt5LIBS="-lQt5Core -lQt5Widgets"

  GPP_64=g++
  INCLUDES+="-I /usr/include/x86_64-linux-gnu/qt5/"
 
  CC_FLAGS="-O2 -Wno-write-strings"
  CC_FLAGS+=" ${INCLUDES}"

  XX_FLAGS="${CC_FLAGS} -fPIC -std=c++2a"
#  XX_FLAGS="${CC_FLAGS} -fPIC -shared -std=c++2a"
  XX_FLAGS+=" ${INCLUDES}"

  GPP_64_REL_FLAGS+="-m64 ${XX_FLAGS} -DRELEASE"

#  SOURCE="./cpp/fpc-qt.cc"
#  CPP_OBJ_DIR="./obj_cpp"
#  TARGET=${CPP_OBJ_DIR}/libfpc-qt.so
#  TARGET_REL_64="${TARGET}"
}


build_shared_library()
{
  echo "${FUNCNAME[0]}"

  pushd "$repo_project_dir/src"
  mkdir -p "obj_cpp"

  #
  # stage 1: compile c-files
  #
  local _sourcefilenames=(cpp/*.cc)
  local _targetfilenames=()

  for _sourcefilename in ${_sourcefilenames[@]}
  do
    _targetfilename="${_sourcefilename%.*}.o"
    _targetfilenames+=("$_targetfilename")
    $GPP_64 -c ${GPP_64_REL_FLAGS} -o ${_targetfilename} ${_sourcefilename}
  done

  #
  # stage 2: link individual objects into shared library
  #
  $GPP_64 ${GPP_64_REL_FLAGS} -shared -o obj_cpp/libfpc-qt.so ${_targetfilenames[@]} ${Qt5LIBS}

  popd
}


build_fpc_project()
{
  echo "${FUNCNAME[0]}"

  # build pascal project
  pushd "$repo_project_dir/src"
  fpc -B -Fl${PWD}/obj_cpp fpcqt.dpr
  popd
}


build()
{
  ${FUNCNAME[0]}_$1
}


extract()
{
  local _filename=$1
  local _fileext="${_filename##*.}"

  echo "file name = $_filename"
  echo "file extention = $_fileext"

  case "$_fileext" in
    "zip") unzip "$_filename" ;;
    *)     echo "extracting $_fileext archives not supported" ;;
  esac
}


fetch()
{
  local _url="$1"
  local _dirname="${_url%/*}"
  local _basename="${_url##*/}"
  local _filename="${_basename%%.*}"
  local _fileext="${_basename##*.}"
  local _url_parts=()
  readarray -td "/" _url_parts <<<"$_dirname"

  repo_project_hash="$_filename"
  repo_project_name="${_url_parts[4]}"
  repo_project_dir="$repo_project_name-$repo_project_hash"

  if [ ! -d "$repo_project_dir" ]; then
    if [ ! -f $_basename ]; then
      wget "$_url"
    fi
    extract "$_basename"
  fi
}


patch()
{
  pushd "$repo_project_dir/src"

  local _filename="pas/misc.pas"
  if [ ! -f "${_filename}.original" ]; then
    echo "copying file that is going to be patched"
    cp "${_filename}" "${_filename}.original"
    sed -i -e 's/fpcso.so/fpc-qt/' $_filename
  fi
  popd
}


main()
{
  mkdir -p "$WRKDIR"
  pushd "$WRKDIR"

  fetch "https://github.com/paule32/fpc-qt/archive/90d880e39621c15c27cf6d3312363d722e5be59e.zip"

  configure

  build shared_library

  # manual patch repo fpc files
  patch
  build fpc_project

  popd
}

WRKDIR="work"
main

echo .
echo "It should now be possible to invoke ./$WRKDIR/$repo_project_dir/src/fpcqt"
echo "if receiving the following error on execution: "
echo "error while loading shared libraries: libfpc-qt.so: cannot open shared object file: No such file or directory"
echo "then export the LD_LIBRARY_PATH from the terminal to include the shared library object directory before execution"
echo "of the compiled executable. e.g:"
echo "export LD_LIBRARY_PATH=$PWD/$WRKDIR/$repo_project_dir/src/obj_cpp:\$LD_LIBRARY_PATH"
echo .
