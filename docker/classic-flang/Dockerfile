FROM classic-flang-llvm-project

LABEL maintainer "Dmitry Mikushin <dmitry@kernelgen.org>"

ENV DEBIAN_FRONTEND noninteractive

# Install pthreads to fix "Could NOT find Threads (missing: Threads_FOUND)".
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libc6-dev \
        gfortran && \
    rm -rf /var/lib/apt/lists/*

COPY ./classic-flang.sh /

