#!/bin/bash
set -e

# Set an LLVM branch Flang to be based on.
# For now, the recommended branch is release_15x, because release_16x is not
# patched yet to support PGI-specific Flang compiler options.
#BRANCH=release_14x
BRANCH=release_15x
#BRANCH=release_16x

# Classic Flang requires modified LLVM.
# Get the source for it, if not yet available.
# We give a way to reuse the existing source to allow our own modifications of it.
if [ ! -e /classic-flang-llvm-project/src/classic-flang-llvm-project ]; then
    mkdir -p /classic-flang-llvm-project/src && \
        cd /classic-flang-llvm-project/src && \
        git clone https://github.com/flang-compiler/classic-flang-llvm-project.git && \
	cd classic-flang-llvm-project
fi

git config --global --add safe.directory /classic-flang-llvm-project/src/classic-flang-llvm-project

# Switch to the requested branch
cd /classic-flang-llvm-project/src && \
    cd classic-flang-llvm-project && \
    git checkout $BRANCH

if [[ -n ${DISTCC_HOSTS} ]]; then
export CCACHE_PREFIX=distcc
echo "Using distcc with DISTCC_HOSTS=\"${DISTCC_HOSTS}\""
PUMP=pump
DISTCC_JOBS=$(echo "${DISTCC_HOSTS}" | egrep -i -o '/[0-9]+' | sed 's|/||g' | xargs  | sed -e 's/\ /+/g' | bc)
DISTCC_JOBS="-- -j ${DISTCC_JOBS}"
fi

# Build (or partially rebuild after modifications) and install LLVM from source.
# Note: we use clang, not gcc - to avoid "LIBOMP: 128-bit quad precision functionality requested but not available"
# No "openmp" in LLVM_ENABLE_PROJECTS - to avoid "add_custom_target cannot create target "check-openmp" because another
# target with the same name already exists", see https://discourse.llvm.org/t/openmp-nvidia-offload-build-problem-13-0-1/60096
mkdir -p /classic-flang-llvm-project/build && \
    cd /classic-flang-llvm-project/build && \
    cmake -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_COMPILER=clang-15 -DCMAKE_CXX_COMPILER=clang++-15 -DCMAKE_LINKER=mold \
    -DCMAKE_INSTALL_PREFIX=/opt/llvm -DLLVM_ENABLE_PROJECTS="clang" -DLLVM_ENABLE_RUNTIMES="openmp" \
    -DLLVM_TARGETS_TO_BUILD="X86;AArch64;NVPTX;AMDGPU" -DLLVM_ENABLE_CLASSIC_FLANG=ON \
    -DLLVM_CCACHE_BUILD=ON \
    /classic-flang-llvm-project/src/classic-flang-llvm-project/llvm && \
    ${PUMP} cmake --build . ${DISTCC_JOBS} && \
    cmake --install .

# Note: classing Flang and in-tree Flang cannot co-exist in the toolchain at this time:
# https://github.com/flang-compiler/classic-flang-llvm-project/issues/158

