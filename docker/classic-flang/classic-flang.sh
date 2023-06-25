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

# Switch to the requested branch
cd /classic-flang/src && \
    cd classic-flang && \
    git checkout $BRANCH

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
# The building is only possible with in-tree LLVM Flang (flang-new), or bootstrapped
# with the classic Flang itself. GNU Fortran fails with some stupid parsing errors.
#
# The C/C++ code of flang2 is so badly writen, that a modern compiler such as clang-16
# is not able to compile it in -Werror mode. The only way to proceed for now is to disable
# -Werror with "-DWITH_WERROR=OFF" and honestly inform the users that their very important
# nuclear physics may melt down because of uninitialized variable inside the compiler code.
#
# If you want to try GNU Fortran, switch from Ninja to "Unix Makefiles" to avoid
# "ninja: build stopped: multiple rules generate include-static/__norm2.mod", and
# change "cmake --build ." to "cmake --build . -- -j$(grep -c ^processor /proc/cpuinfo)"
mkdir -p /classic-flang/build && \
    cd /classic-flang/build && \
    mkdir -p flang && \
    cd flang && \
    cmake -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_CONFIG=/opt/llvm/bin/llvm-config -DCMAKE_PREFIX_PATH=/opt/llvm \
    -DCMAKE_C_COMPILER=/opt/llvm/bin/clang -DCMAKE_CXX_COMPILER=/opt/llvm/bin/clang++ \
    -DCMAKE_Fortran_COMPILER=/opt/llvm/bin/flang-new -DCMAKE_Fortran_COMPILER_ID=Flang -DCMAKE_LINKER=mold \
    -DCMAKE_INSTALL_PREFIX=/opt/flang -DLLVM_TARGETS_TO_BUILD="X86;AArch64" -DFLANG_OPENMP_GPU_NVIDIA=ON \
    -DFLANG_INCLUDE_DOCS=ON -DFLANG_LLVM_EXTENSIONS=ON -DWITH_WERROR=OFF \
    /classic-flang/src/classic-flang && \
    cmake --build . && \
    cmake --install .

