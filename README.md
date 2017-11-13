Flang
=====

Flang is a Fortran compiler targeting LLVM.

Please join us for a Flang meetup at SC 17 in Denver. NVIDIA has reserved a room for the meetup on Wednesday, November 15 at 6:30 PM at the Curtis Hotel in the Hopscotch room (3rd floor). The Curtis is a block from the convention center. Light refreshments will be provided! (And maybe snacks, too.) We are looking forward to seeing you all face-to-face.

We have mailing lists for announcements and developers. Here's the link with the sign-up information:

http://lists.flang-compiler.org/mailman/listinfo

We have a flang-compiler channel on Slack.  Slack is invitation only but anyone can join.  Here's the link:

https://join.slack.com/t/flang-compiler/shared_invite/MjExOTEyMzQ3MjIxLTE0OTk4NzQyNzUtODQzZWEyMjkwYw

## Building Flang

We build Flang on Intel x86-64 and OpenPOWER hardware running either Ubuntu or Red Hat.

## Prerequisites

Building LLVM requires fairly modern compiler toolchain and CMake, check [Getting started with LLVM](http://llvm.org/releases/4.0.0/docs/GettingStarted.html#host-c-toolchain-both-compiler-and-standard-library) and [Building LLVM with CMake][llvm-cmake] for the full list. 

## Dependencies

- LLVM
- openmp-llvm
- modified clang

The latest supported LLVM version is 4.0.  Flang also supports LLVM version 5.0 and 3.9.  To use 5.0, substitute 50 for 40 in the build instructions.  To use 3.9, substitute 39 for 40 in the build instructions.

## Building

#### Custom install location

The command-line examples provided below will install everything into the default system location.

To specify a custom install location, add `-DCMAKE_INSTALL_PREFIX=<INSTALL_PREFIX>` to each of the CMake commands given below.

When using a custom install location, you must make sure that the bin directory is on your PATH when building and running flang.

Flang is developed outside of the llvm source tree.

#### Step-by-step instructions

1. Get LLVM 4.0, build and install it according to [instructions][llvm-cmake]
   ```
   cd where/you/want/to/build
   git clone https://github.com/llvm-mirror/llvm.git
   cd llvm
   git checkout release_40
   mkdir build && cd build
   cmake ..
   make 
   sudo make install
   ```

2. Get the modified clang for flang, build and install it (4.0)
   ```
   cd where/you/want/to/build
   git clone https://github.com/flang-compiler/clang.git
   cd clang
   git checkout flang_release_40
   mkdir build && cd build
   cmake ..
   make
   sudo make install
   ```
   
   If you use `CMAKE_INSTALL_PREFIX` in Step 1 and `<INSTALL_PREFIX>/bin` is not in your path, you need to add `-DLLVM_CONFIG=<INSTALL_PREFIX>/bin/llvm-config` when invoking CMake, otherwise you will encounter an error related to `LLVMConfig.cmake` not being found.

3. Build and install openmp-llvm (4.0)
   ```
   cd where/you/want/to/build
   git clone https://github.com/llvm-mirror/openmp.git
   cd openmp/runtime
   git checkout release_40
   mkdir build && cd build
   cmake ..
   make
   sudo make install
   ```

4. Build and install the flang components
   ```
   cd where/you/want/to/build
   git clone https://github.com/flang-compiler/flang.git
   cd flang
   mkdir build && cd build
   cmake -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DCMAKE_Fortran_COMPILER=flang ..
   make
   sudo make install
   ```
   
   If you use `CMAKE_INSTALL_PREFIX` in Step 1 and `<INSTALL_PREFIX>/bin` is not in your path, you need to add `-DLLVM_CONFIG=<INSTALL_PREFIX>/bin/llvm-config` and set the explicit paths for the compilers (i.e. ` -DCMAKE_CXX_COMPILER=<INSTALL_PREFIX>/bin/clang++ -DCMAKE_C_COMPILER=<INSTALL_PREFIX>/bin/clang -DCMAKE_Fortran_COMPILER=<INSTALL_PREFIX>/bin/flang`) when invoking CMake, otherwise you will encounter errors.

[llvm-cmake]: http://llvm.org/releases/4.0.0/docs/CMake.html

#### (Alternative:) Build flang using Spack

[Spack](https://github.com/LLNL/spack) is a flexible package manager for HPC system and can be used to build flang and its dependencies.

1. Get Spack
   ```
   git clone https://github.com/llnl/spack.git
   ```
   
   On bash:
   ```
   source spack/share/spack/setup-env.sh
   ```
   or tcsh:
   ```
   source spack/share/spack/setup-env.csh
   ```
   

2. Build Flang and its depencencies:
   ```
   spack install flang
   ```
   watch out for the installation path, the flang wrapper script in there is ready to use.

3. (optional) setup Flang as a compiler inside spack if you want to build spack package with flang:
   ```
   spack compiler add path/to/flang/bin
   ```
   Now you might want to edit your `~/.spack/*/compilers.yaml` to combine `flang` with any c compiler of your choice.
   

## Using Flang

To test your installation, create a simple "hello world" program, like the following:

```
program hello
  print *, 'hello world'
end
```

Next, compile the program in the following manner. We will assume the program is called hello.f90

```
% flang hello.f90
```

If the build succeeds, then you can execute the program in the following manner:

```
% ./a.out
```

## Compiler Options

For a list of compiler options, enter

```
% flang -help
```

The Flang compiler supports accepts all clang 4.0 compiler options and supports many, as well as the following flang-specific compiler options:

```lang-none
-noFlangLibs          Do not link against Flang libraries
-mp                   Enable OpenMP and link with with OpenMP library libomp
-nomp                 Do not link with OpenMP library libomp
-Mbackslash           Treat backslash character like a C-style escape character
-Mno-backslash        Treat backslash like any other character
-Mbyteswapio          Swap byte-order for unformatted input/output
-Mfixed               Assume fixed-format source
-Mextend              Allow source lines up to 132 characters
-Mfreeform            Assume free-format source
-Mpreprocess          Run preprocessor for Fortran files
-Mrecursive           Generate code to allow recursive subprograms
-Mstandard            Check standard conformance
-Msave                Assume all variables have SAVE attribute
-module               path to module file (-I also works)
-Mallocatable=95      Select Fortran 95 semantics for assignments to allocatable objects (Default)
-Mallocatable=03      Select Fortran 03 semantics for assignments to allocatable objects
-static-flang-libs    Link using static Flang libraries
-M[no]daz             Treat denormalized numbers as zero
-M[no]flushz          Set SSE to flush-to-zero mode
-Mcache_align         Align large objects on cache-line boundaries
-M[no]fprelaxed       This option is ignored
-fdefault-integer-8   Treat INTEGER and LOGICAL as INTEGER*8 and LOGICAL*8
-fdefault-real-8      Treat REAL as REAL*8
-i8                   Treat INTEGER and LOGICAL as INTEGER*8 and LOGICAL*8
-r8                   Treat REAL as REAL*8
-fno-fortran-main     Don't link in Fortran main
```

