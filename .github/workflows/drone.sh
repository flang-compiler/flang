apt update
apt install -y python3-sphinx cmake curl jq wget
cmake --version
make --version
gcc --version
cd ../..
curl -sL https://api.github.com/repos/flang-compiler/llvm/actions/workflows/build_llvm.yml/runs --output runs_llvm.json
curl -sL https://api.github.com/repos/flang-compiler/flang-driver/actions/workflows/build_flang-driver.yml/runs --output runs_flang-driver.json
wget --output-document artifacts_llvm `jq -r '.workflow_runs[0].artifacts_url?' runs_llvm.json`
wget --output-document artifacts_flang-driver `jq -r '.workflow_runs[0].artifacts_url?' runs_flang-driver.json`
echo "cat artifacts_llvm"
cat artifacts_llvm
i="0"
set -x
while [ `jq -r '.total_count?' artifacts_llvm` == "0" ] && [ $i -lt 3 ]
do
    echo "No artifacts in build $i, counting from latest" 
    i=$[$i+1] 
    wget --output-document artifacts_llvm `jq -r --argjson i "$i" '.workflow_runs[$i].artifacts_url?' runs_llvm.json` 
    echo "cat artifacts_llvm" 
    cat artifacts_llvm 
done
echo "cat artifacts_flang-driver"
cat artifacts_flang-driver
i="0"
while [ `jq -r '.total_count?' artifacts_flang-driver` == "0" ] && [ $i -lt 3 ]
do
    echo "No artifacts in build $i, counting from latest" 
    i=$[$i+1] 
    wget --output-document artifacts_flang-driver `jq -r --argjson i "$i" '.workflow_runs[$i].artifacts_url?' runs_flang-driver.json` 
    echo "cat artifacts_flang-driver" 
    cat artifacts_flang-driver 
done
url=`jq -r '.artifacts[] | select(.name == "llvm_build_arm64") | .archive_download_url' artifacts_llvm`
url=`jq -r '.artifacts[] | select(.name == "flang-driver_build_arm64") | .archive_download_url' artifacts_flang-driver`
unzip llvm_build.zip
tar xzf llvm_build.tar.gz
cd llvm/build
sudo make install/fast
cd ../..
unzip flang-driver_build.zip
tar xzf flang-driver_build.tar.gz
cd flang-driver/build
sudo make install/fast
flang --version
CMAKE_OPTIONS="-DLLVM_TARGETS_TO_BUILD=OpenMP -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=/usr/bin/gcc -DCMAKE_CXX_COMPILER=/usr/bin/g++"
git clone --depth 1 --single-branch --branch release_90 https://github.com/llvm-mirror/openmp.git
cd openmp
mkdir -p build && cd build
cmake $CMAKE_OPTIONS ..
make -j$(nproc)
make install
mkdir -p build && cd build
cmake -DLLVM_TARGETS_TO_BUILD=flang -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=/usr/bin/gcc -DCMAKE_CXX_COMPILER=/usr/bin/g++ -DCMAKE_Fortran_COMPILER=/usr/local/bin/flang -DCMAKE_Fortran_COMPILER_ID=Flang -DFLANG_INCLUDE_DOCS=ON -DFLANG_LLVM_EXTENSIONS=ON ke -j$(nproc)
make install
