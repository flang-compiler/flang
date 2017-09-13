# Use Fedora as it has llvm-4 packages that seem to work
FROM fedora

RUN dnf -y update && dnf clean all
RUN dnf -y install clang-4.0.1 llvm-4.0.1 llvm-devel-4.0.1 \
                   llvm-libs llvm-static ncurses-devel zlib-devel \
                   git cmake which \
    && dnf clean all

# Modified clang
RUN mkdir -p /opt/src/
RUN git clone https://github.com/flang-compiler/clang.git /opt/src/clang \
 && cd /opt/src/clang \
 && git checkout flang_release_40 \
 && mkdir build 
WORKDIR /opt/src/clang/build
RUN cmake .. 
RUN make -j 4 
RUN make install

RUN git clone https://github.com/llvm-mirror/openmp.git /opt/src/openmp \
 && cd /opt/src/openmp/runtime \
 && git checkout release_40 \
 && mkdir build 
WORKDIR /opt/src/openmp/runtime/build
RUN cmake ..
RUN make -j 4
RUN make install

RUN git clone https://github.com/flang-compiler/flang.git /opt/src/flang \
 && cd /opt/src/flang \
 && mkdir build
WORKDIR /opt/src/flang/build
RUN sed -i 's|  set(LLVM_CMAKE_PATH "${LLVM_BINARY_DIR}/lib/cmake/llvm")|  set(LLVM_CMAKE_PATH "${LLVM_BINARY_DIR}/lib64/cmake/llvm")|' ../CMakeLists.txt 
RUN cmake -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DCMAKE_Fortran_COMPILER=flang ..
RUN make
RUN make install

RUN echo -e "/usr/local/lib\n/usr/local/lib64" > /etc/ld.so.conf.d/flang.conf
RUN ldconfig

RUN echo -e "program hello\n  print *, 'hello world'\n end" > hello.f90
RUN flang -o hello hello.f90
RUN ./hello
