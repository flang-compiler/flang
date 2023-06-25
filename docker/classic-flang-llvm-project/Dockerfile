# We use distcc to accelerate the building process.
# Compile distcc from source, because distcc Ubuntu package
# is broken in multiple ways.

FROM ubuntu:22.04 as distcc

LABEL maintainer "Dmitry Mikushin <dmitry@kernelgen.org>"

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        build-essential \
        libiberty-dev \
        automake \
        autoconf \
        python3 \
        python3-dev \
        git && \
    rm -rf /var/lib/apt/lists/*

RUN useradd -rm -s /bin/bash distcc

USER distcc

WORKDIR /home/distcc

RUN git clone https://github.com/distcc/distcc.git && \
    cd distcc && \
    ./autogen.sh && \
    ./configure --prefix=/usr/local && \
    make clean && \
    make

USER root

RUN cd distcc && \
    make install

USER distcc

RUN cd distcc && \
    PATH=$PATH:/distcc/usr/bin make check

#########################################################

FROM ubuntu:22.04

LABEL maintainer "Dmitry Mikushin <dmitry@kernelgen.org>"

ENV DEBIAN_FRONTEND noninteractive

# Install distcc
COPY --from=distcc /usr/local/. /usr/local

# Install build dependencies of LLVM: compiler, python and git.
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        build-essential \
        pkg-config \
        python3 \
        python3-dev \
        cmake \
        ccache \
        zlib1g-dev \
        wget \
        unzip \
        git \
        clang-15 \
        libffi-dev \
        zstd \
        bc \
        libhsa-runtime-dev && \
    rm -rf /var/lib/apt/lists/*

# Install libcuda.so for LIBOMPTARGET_DEP_CUDA_DRIVER.
# Extract just this single file, because the whole package is big and useless.
RUN apt-get update && \
    apt download nvidia-cuda-dev && \
    ar vx nvidia-cuda-dev* && \
    tar -xvf data.tar.zst -C . ./usr/lib/x86_64-linux-gnu/stubs/libcuda.so && \
    mv ./usr/lib/x86_64-linux-gnu/stubs/libcuda.so /usr/lib/x86_64-linux-gnu/ && \
    rm -rf ./usr/lib/x86_64-linux-gnu/stubs && \
    rm -rf /var/lib/apt/lists/*

# Install a newer ninja release. It seems the older version in the debian repos
# randomly crashes when compiling llvm.
# Fortran support in Ninja starts with v1.10
RUN wget "https://github.com/ninja-build/ninja/releases/download/v1.11.1/ninja-linux.zip" && \
    echo "b901ba96e486dce377f9a070ed4ef3f79deb45f4ffe2938f8e7ddc69cfb3df77  ninja-linux.zip" \
        | sha256sum -c  && \
    unzip ninja-linux.zip -d /usr/local/bin && \
    rm ninja-linux.zip

# Install mold for faster linking 
RUN apt-get update && \
     git clone https://github.com/rui314/mold.git && \
     mkdir mold/build && \
     cd mold/build && \
     git checkout v1.7.1 && \
     ../install-build-deps.sh && \
     cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=c++ -G Ninja .. && \
     cmake --build . && \
     cmake --install . && \
     rm -rf /var/lib/apt/lists/*

COPY ./classic-flang-llvm-project.sh /

