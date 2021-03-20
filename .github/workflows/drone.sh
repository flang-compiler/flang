
apt-get update
apt-get install -y python3-sphinx cmake curl jq wget -q

git --version
cmake --version
make --version
gcc --version

cd ../..
curl -sL https://api.github.com/repos/flang-compiler/llvm/actions/workflows/build_llvm.yml/runs --output runs_llvm.json
curl -sL https://api.github.com/repos/flang-compiler/flang-driver/actions/workflows/build_flang-driver.yml/runs --output runs_flang-driver.json

wget --output-document artifacts_llvm `jq -r '.workflow_runs[0].artifacts_url?' runs_llvm.json`
wget --output-document artifacts_flang-driver `jq -r '.workflow_runs[0].artifacts_url?' runs_flang-driver.json`

# Retry with previous build in case no artifacts are available 
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

url=`jq -r '.artifacts[] | select(.name == "llvm_build_ARM64") | .archive_download_url' artifacts_llvm`
wget --output-document llvm_build.zip --header="Authorization: Bearer " $url

url=`jq -r '.artifacts[] | select(.name == "flang-driver_build_ARM64") | .archive_download_url' artifacts_flang-driver`
wget --output-document flang-driver_build.zip --header="Authorization: Bearer " $url