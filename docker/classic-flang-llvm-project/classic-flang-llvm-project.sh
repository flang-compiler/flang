#!/bin/bash
set -e

# Set an LLVM branch Flang to be based on.
# The latest supported LLVM version is 10.0: https://github.com/flang-compiler/flang/wiki/Building-Flang
# TODO We would want to upgrade to LLVM version 16, in order to support recent AMD GPUs.
BRANCH=release_100
#BRANCH=release/16.x

# Classic Flang requires modified LLVM.
# Get the source cfor it, if not yet available.
# We give a way to reuse the existing source to allow our own modifications of it.
if [ ! -e /classic-flang-llvm-project/src/classic-flang-llvm-project ]; then
    mkdir -p /classic-flang-llvm-project/src && \
        cd /classic-flang-llvm-project/src && \
        git clone https://github.com/flang-compiler/classic-flang-llvm-project.git && \
	cd classic-flang-llvm-project && \
	git checkout $BRANCH
fi

# Build (or partially rebuild after modifications) and install LLVM from source.
mkdir -p /classic-flang-llvm-project/build && \
    cd /classic-flang-llvm-project/build && \
    cmake -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_LINKER=mold -DCMAKE_C_FLAGS=-fuse-ld=mold -DCMAKE_CXX_FLAGS=-fuse-ld=mold \
    -DCMAKE_INSTALL_PREFIX=/opt/llvm -DLLVM_TARGETS_TO_BUILD="X86;AArch64;NVPTX;AMDGPU" \
    /classic-flang-llvm-project/src/classic-flang-llvm-project/llvm && \
    cmake --build . && \
    cmake --install .

