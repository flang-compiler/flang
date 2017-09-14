# Use Fedora as it has llvm-4 packages that seem to work
FROM fedora

RUN dnf -y update && dnf clean all
RUN dnf -y install llvm-4.0.1 llvm-devel-4.0.1 llvm-libs-4.0.1 \
                   llvm-static-4.0.1 clang-4.0.1 \
                   ncurses-devel zlib-devel \
                   git cmake which findutils \
    && dnf clean all

ENV CMAKE_BUILD_TYPE Release
ENV MAKE_JOBS 4

# Modified clang
RUN mkdir -p /opt/src/ \
 && git clone https://github.com/flang-compiler/clang.git /opt/src/clang \
 && cd /opt/src/clang \
 && git checkout flang_release_40 \
 && mkdir build && cd build \
 && cmake .. -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} \
 && make -j ${MAKE_JOBS} \
 && make install \
 && rm -rf /opt/src/clang

