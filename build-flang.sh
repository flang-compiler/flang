#!/bin/bash

# A POSIX variable
OPTIND=1         # Reset in case getopts has been used previously in the shell.

# Initialize our own variables:
TARGET="X86"
BUILD_TYPE="Release"
BUILD_PREFIX="./build"
INSTALL_PREFIX="/usr/local"
CROSS_TARGET=""
NPROC=1
USE_LLVM_MAIN_SRC_DIR=""
LLVM_CONFIG_BIN=""
USE_CCACHE="0"
USE_SUDO="0"
EXTRA_CMAKE_OPTS=""
VERBOSE=""

set -e # Exit the script on first error.

function print_usage {
    echo "Usage: ./build-flang.sh [options]";
    echo "";
    echo "Build and install libpgmath and flang.";
    echo "Run this script in a directory with flang sources.";
    echo "Example:";
    echo "  $ git clone https://github.com/flang-compiler/flang";
    echo "  $ cd flang";
    echo "  $ ./build-flang.sh -t X86 -p /install/prefix/ -n 2 -s";
    echo "";
    echo "Options:";
    echo "  -t  Target to build for (X86, AArch64, PowerPC). Default: X86";
    echo "  -d  CMake build type. Default: Release";
    echo "  -b  Build prefix. Default: ./build";
    echo "  -p  Install prefix. Default: /usr/local";
    echo "  -X  Build a cross-compiler for given target triple. Default: N/A"
    echo "  -n  Number of parallel jobs. Default: 1";
    echo "  -l  Path to LLVM sources. Default: not set";
    echo "  -o  Path to llvm-config. Default: not set";
    echo "  -c  Use ccache. Default: 0 - do not use ccache";
    echo "  -s  Use sudo to install. Default: 0 - do not use sudo";
    echo "  -x  Extra CMake options. Default: ''";
    echo "  -v  Enable verbose output";
}

while getopts "t:X:d:b:p:n:l:o:csx:v?" opt; do
    case "$opt" in
        t) TARGET=$OPTARG;;
        d) BUILD_TYPE=$OPTARG;;
        b) BUILD_PREFIX=$OPTARG;;
        p) INSTALL_PREFIX=$OPTARG;;
        X) CROSS_TARGET=$OPTARG;;
        n) NPROC=$OPTARG;;
        l) USE_LLVM_MAIN_SRC_DIR="-DLLVM_MAIN_SRC_DIR=$OPTARG";;
        o) LLVM_CONFIG_BIN="$OPTARG";;
        c) USE_CCACHE="1";;
        s) USE_SUDO="1";;
        x) EXTRA_CMAKE_OPTS="$OPTARG";;
        v) VERBOSE="1";;
        ?) print_usage; exit 0;;
    esac
done

CMAKE_OPTIONS="-DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX \
    -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
    -DCMAKE_AR=$INSTALL_PREFIX/bin/llvm-ar \
    -DCMAKE_C_COMPILER=$INSTALL_PREFIX/bin/clang \
    -DCMAKE_CXX_COMPILER=$INSTALL_PREFIX/bin/clang++ \
    -DCMAKE_RANLIB=$INSTALL_PREFIX/bin/llvm-ranlib \
    -DLLVM_TARGETS_TO_BUILD=$TARGET \
    $USE_LLVM_MAIN_SRC_DIR"

# Use lld for release_19x or newer version of classic-flang-llvm-project.
set -x
if [ -n "$LLVM_CONFIG_BIN" ]; then
  CMAKE_OPTIONS="$CMAKE_OPTIONS -DLLVM_CONFIG=$LLVM_CONFIG_BIN"
  LLVM_MAJOR_VERSION=$("$LLVM_CONFIG_BIN" --version | cut -f1 -d.)
  if [ "$LLVM_MAJOR_VERSION" -gt 19 ]; then
    CMAKE_OPTIONS="$CMAKE_OPTIONS \
        -DCMAKE_EXE_LINKER_FLAGS=-fuse-ld=lld \
        -DCMAKE_SHARED_LINKER_FLAGS=-fuse-ld=lld"
  fi
fi
set +x

if [ $USE_CCACHE == "1" ]; then
  echo "Build using ccache"
  CMAKE_OPTIONS="$CMAKE_OPTIONS \
      -DCMAKE_C_COMPILER_LAUNCHER=ccache \
      -DCMAKE_CXX_COMPILER_LAUNCHER=ccache"
fi

if [ -n "$CROSS_TARGET" ]; then
  CMAKE_OPTIONS="$CMAKE_OPTIONS \
      -DCROSS_COMPILE_TARGET_TRIPLE=$CROSS_TARGET \
      -DLLVM_DEFAULT_TARGET_TRIPLE=$CROSS_TARGET \
      -DLLVM_ENABLE_PER_TARGET_RUNTIME_DIR=ON"
fi

if [ -n "$EXTRA_CMAKE_OPTS" ]; then
  CMAKE_OPTIONS="$CMAKE_OPTIONS $EXTRA_CMAKE_OPTS"
fi

# Assume the script is run where it is located.
TOPDIR=$PWD
if [ "${BUILD_PREFIX:0:1}" != "/" ]; then
  BUILD_PREFIX="$TOPDIR/$BUILD_PREFIX"
fi

# Build and install libpgmath.
mkdir -p $BUILD_PREFIX/libpgmath && cd $BUILD_PREFIX/libpgmath
if [ -n "$VERBOSE" ]; then
  set -x
fi
if [ -n "$CROSS_TARGET" ]; then
  cmake $CMAKE_OPTIONS \
    -DCMAKE_C_COMPILER_TARGET=$CROSS_TARGET \
    -DCMAKE_CXX_COMPILER_TARGET=$CROSS_TARGET \
    $TOPDIR/runtime/libpgmath
else
  cmake $CMAKE_OPTIONS $TOPDIR/runtime/libpgmath
fi
set +x
make -j$NPROC VERBOSE=$VERBOSE
if [ $USE_SUDO == "1" ]; then
  echo "Install with sudo"
  sudo make install VERBOSE=$VERBOSE
else
  echo "Install without sudo"
  make install VERBOSE=$VERBOSE
fi

# Build and install flang.
mkdir -p $BUILD_PREFIX/flang && cd $BUILD_PREFIX/flang
if [ -n "$VERBOSE" ]; then
  set -x
fi
cmake $CMAKE_OPTIONS \
      -DCMAKE_Fortran_COMPILER=$INSTALL_PREFIX/bin/flang \
      -DCMAKE_Fortran_COMPILER_ID=Flang \
      -DFLANG_INCLUDE_DOCS=ON \
      -DFLANG_LLVM_EXTENSIONS=ON \
      -DWITH_WERROR=ON \
      $TOPDIR
set +x
make -j$NPROC VERBOSE=$VERBOSE
if [ $USE_SUDO == "1" ]; then
  echo "Install with sudo"
  sudo make install VERBOSE=$VERBOSE
else
  echo "Install without sudo"
  make install VERBOSE=$VERBOSE
fi
