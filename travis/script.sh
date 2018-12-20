set -x
ROOT=`pwd`
PREFIX=$ROOT/install

# Build LLVM

cd $ROOT
git clone https://github.com/flang-compiler/llvm.git
cd llvm
git checkout release_60
mkdir build && cd build
cmake ../ -DCMAKE_INSTALL_PREFIX=$PREFIX -DCMAKE_BUILD_TYPE=Release
make install -j32

# Build flang driver
cd $ROOT
git clone https://github.com/flang-compiler/flang-driver.git
cd flang-driver
git checkout release_60
mkdir build && cd build
cmake ../ -DCMAKE_INSTALL_PREFIX=$PREFIX -DLLVM_CONFIG=$PREFIX/bin/llvm-config
nice -n 18 make install -j32

# Build OpenMP
cd $ROOT
git clone https://github.com/llvm-mirror/openmp.git
cd openmp/runtime
git checkout release_60
mkdir build-openmp && cd build-openmp
cmake ../../ -DCMAKE_INSTALL_PREFIX=$PREFIX
nice -n 18 make -j32 install

# get flang source
cd $ROOT
git clone https://github.com/flang-compiler/flang.git

# Build pgmath with clang
CLANG=$PREFIX/bin/clang
CPPCLANG=$PREFIX/bin/clang++
cd $ROOT
cd flang/runtime/libpgmath
mkdir build && cd build
cmake ../ -DCMAKE_INSTALL_PREFIX=$PREFIX -DCMAKE_C_COMPILER=$CLANG -DCMAKE_CXX_COMPILER=$CPPCLANG
nice -n 18 make -j32 install

# Build flang
cd $ROOT

#git clone https://github.com/flang-compiler/flang.git
cd flang
mkdir build && cd build
export LD_LIBRARY_PATH=$PREFIX/lib:$LD_LIBRARY_PATH
cmake ../  -DCMAKE_INSTALL_PREFIX=$PREFIX -DLLVM_CONFIG=$PREFIX/bin/llvm-config -DCMAKE_Fortran_COMPILER=$PREFIX/bin/flang  -DCMAKE_C_COMPILER=$CLANG -DCMAKE_CXX_COMPILER=$CPPCLANG
nice -n 18 make -j32 install


#CHECK IT

cp $ROOT/llvm/build/bin/llvm-lit $ROOT/flang/build/bin/
make check-all
