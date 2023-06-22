#!/bin/bash
set -e

BRANCH=master

# Get the Flang source, if not yet available.
if [ ! -e /classic-flang/src/classic-flang ]; then
    mkdir -p /classic-flang/src && \
        cd /classic-flang/src && \
        git clone https://github.com/flang-compiler/flang.git classic-flang && \
	cd classic-flang && \
	git checkout $BRANCH
fi

# Build pgmath prerequisite.
mkdir -p /classic-flang/build && \
    cd /classic-flang/build && \
    mkdir -p pgmath && \
    cd pgmath && \
    cmake -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_COMPILER=/opt/llvm/bin/clang -DCMAKE_CXX_COMPILER=/opt/llvm/bin/clang++ \
    -DCMAKE_INSTALL_PREFIX=/opt/flang \
    /classic-flang/src/classic-flang/runtime/libpgmath && \
    cmake --build . && \
    cmake --install .

# Build (or partially rebuild after modifications) and install Flang from source.
# -DCMAKE_Fortran_COMPILER=/opt/llvm/bin/flang-new -DCMAKE_Fortran_COMPILER_ID=Flang
# Using makefiles to avoid "ninja: build stopped: multiple rules generate include-static/__norm2.mod"
mkdir -p /classic-flang/build && \
    cd /classic-flang/build && \
    mkdir -p flang && \
    cd flang && \
    cmake -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_CONFIG=/opt/llvm/bin/llvm-config -DCMAKE_PREFIX_PATH=/opt/llvm \
    -DCMAKE_C_COMPILER=/opt/llvm/bin/clang -DCMAKE_CXX_COMPILER=/opt/llvm/bin/clang++ \
    -DCMAKE_Fortran_COMPILER=/opt/llvm/bin/flang-new -DCMAKE_Fortran_COMPILER_ID=Flang -DCMAKE_LINKER=mold \
    -DCMAKE_INSTALL_PREFIX=/opt/flang -DLLVM_TARGETS_TO_BUILD="X86;AArch64" -DFLANG_OPENMP_GPU_NVIDIA=ON \
    /classic-flang/src/classic-flang && \
    cmake --build . -- -j$(grep -c ^processor /proc/cpuinfo) && \
    cmake --install .

