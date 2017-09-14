# clang is built in the Stage 1 image
FROM tdd20/flang-temp

ENV CMAKE_BUILD_TYPE Release
ENV MAKE_JOBS 4

# OpenMP required by flang
RUN git clone https://github.com/llvm-mirror/openmp.git /opt/src/openmp \
 && cd /opt/src/openmp/runtime \
 && git checkout release_40 \
 && mkdir build && cd build \
 && cmake .. -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} \
 && make -j ${MAKE_JOBS} \
 && make install \
 && rm -rf /opt/src/openmp

# Compile flang with modified clang
RUN git clone https://github.com/flang-compiler/flang.git /opt/src/flang \
 && cd /opt/src/flang \
 && mkdir build && cd build \
 && sed -i 's|  set(LLVM_CMAKE_PATH "${LLVM_BINARY_DIR}/lib/cmake/llvm")|  set(LLVM_CMAKE_PATH "${LLVM_BINARY_DIR}/lib64/cmake/llvm")|' ../CMakeLists.txt \
 && cmake -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DCMAKE_Fortran_COMPILER=flang -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} .. \
 && make -j ${MAKE_JOBS} \
 && make install \
 && rm -rf /opt/src/flang

# Add to linker path so that binaries can find the libraries
RUN echo -e "/usr/local/lib\n/usr/local/lib64" > /etc/ld.so.conf.d/flang.conf
RUN ldconfig

# Test if it works
WORKDIR /root/
RUN echo -e "program hello\n  print *, 'hello world'\n end" > hello.f90
RUN flang -o hello hello.f90
RUN ./hello

CMD ['/bin/bash']
