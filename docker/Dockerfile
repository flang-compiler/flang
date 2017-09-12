FROM ubuntu

RUN apt-get -y update \
    && apt-get upgrade -y \
    && apt-get install -y \
        cmake build-essential git python

RUN mkdir -p /opt/src/
RUN git clone https://github.com/llvm-mirror/llvm.git /opt/src/llvm \
 && cd /opt/src/llvm \
 && git checkout release_40 \
 && mkdir build
WORKDIR /opt/src/llvm/build
RUN cmake .. 
RUN make
RUN make install

#RUN mkdir -p /opt/src/
#RUN git clone https://github.com/flang-compiler/clang.git /opt/src/clang \
# && cd /opt/src/clang \
# && git checkout flang_release_40 \
# && mkdir build 
#WORKDIR /opt/src/clang/build
#RUN update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-4.0 100
#RUN update-alternatives --install /usr/bin/clang clang /usr/bin/clang-4.0 100
#RUN update-alternatives --install /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-4.0 100

#RUN cmake -DCMAKE_CXX_COMPILER=clang++-4.0 -DCMAKE_C_COMPILER=clang-4.0 ..
#RUN make -j 4 
#RUN make install


