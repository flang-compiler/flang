FROM ubuntu:22.04

LABEL maintainer "Dmitry Mikushin <dmitry@kernelgen.org>"

ENV DEBIAN_FRONTEND noninteractive

# Install build dependencies of llvm.

# Install compiler, python and git.
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates gnupg \
        build-essential \
        cmake \
        python3 \
        zlib1g-dev \
        wget \
        unzip \
        git && \
    rm -rf /var/lib/apt/lists/*

# Install a newer ninja release. It seems the older version in the debian repos
# randomly crashes when compiling llvm.
RUN wget "https://github.com/ninja-build/ninja/releases/download/v1.8.2/ninja-linux.zip" && \
    echo "d2fea9ff33b3ef353161ed906f260d565ca55b8ca0568fa07b1d2cab90a84a07 ninja-linux.zip" \
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

