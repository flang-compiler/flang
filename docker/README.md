# Build Flang from source in Docker

[Flang](https://github.com/flang-compiler/flang) (also known as "Classic Flang") is an out-of-tree Fortran compiler targeting LLVM.

This builds the Flang compiler in Ubuntu 22.04 Docker container, according to the instructions given on GitHub, plus OpenMP offload features for AMD and NVIDIA GPUs.

Classic Flang build must follow the following sequence.

First, the [Classic Flang LLVM Project](https://github.com/flang-compiler/classic-flang-llvm-project) must be build with `-DLLVM_ENABLE_CLASSIC_FLANG=ON` and `-DLLVM_ENABLE_PROJECTS="clang"` options. These options and patches to the mainline LLVM applied in [Classic Flang LLVM Project](https://github.com/flang-compiler/classic-flang-llvm-project) will prepare a toolchain that could incorporate Classic Flang, when it is built. Specifically, options and patches add support for legacy PGI options such as `-Mreentrant` and call the `flang1` and `flang2` stages, when Fortran compiler is invoked. Note: this scenario completely replaces ("predates") the official [LLVM Flang](https://flang.llvm.org/docs/): it can't be enabled and used together with Classic Flang.

Second, the [Flang](https://github.com/flang-compiler/flang) project must be built with the LLVM toolchain prepared above. CMake configuration must include `flang` as a Fortran compiler, which refers to the Classic Flang frontend enabled by the patches. This Fortran compiler is not yet usable, so we also provide `-DCMAKE_Fortran_COMPILER_ID=Flang`, which instructs CMake to assume Fortran compiler works without checking it. During the build, `flang1` and `flang2` executables will be created to get a working Flang frontend, and then this frontend will be deployed to compile runtime libraries in Fortran language.

Please find more details on the build process in [classic-flang-llvm-project.sh](classic-flang-llvm-project/classic-flang-llvm-project.sh) and [classic-flang.sh](classic-flang/classic-flang.sh).

## Building

Build Flang with the following command:

```
docker-compose up --build
```

Note that the command above will store source files, build intermediates and installation folders as Docker mounted volumes. This method greatly speeds up partial rebuilds, but may require you to manually clean the build folders, if you want to try something completely different (e.g. a different LLVM branch).

The status of build is not printed right into the current terminal window. In order to watch the build log, run the following command in another terminal:

```
docker-compose logs -f
```

## Using distcc

This build supports distributed parallel build with distcc as an extra option.

The distcc hosts shall be provided on ports of a centralized gateway server with an SSH access to it. Gateway configuration is not covered by this instruction and should be provided by the user, e.g. by setting up SSH port forwarding.

1. Create SSH keys in the docker folder:

```
ssh-keygen -t rsa -b 4096 -C "autossh" -f id_rsa
```

2. Create a gateway server account, and use `id_rsa.pub` as an authorized key for it

3. Define distcc host in .env, for example:

```
DISTCC_HOSTS="localhost/8 tunnels:3632/8,lzo,cpp"
```

4. Build Flang as usual and enjoy the distributed compilation being deployed!


Timing of distributed compilation on 4 machines with 8, 8, 8 and 4 cores:

```
time docker-compose up --build
________________________________________________________
Executed in   22.79 mins    fish           external
   usr time   51.68 secs  836.00 micros   51.68 secs
   sys time    2.17 secs  203.00 micros    2.17 secs
```

The same compilation on a local machine with 8 cores:

```
time docker-compose up --build
________________________________________________________
Executed in   53.55 mins    fish           external
   usr time  127.28 secs   26.23 millis  127.25 secs
   sys time    5.02 secs    0.35 millis    5.02 secs
```
