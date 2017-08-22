Flang
=====

Flang is a Fortran compiler targeting LLVM.

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

The latest supported LLVM version is 4.0.  Flang also supports LLVM version 3.9.  To use 3.9, substitute 39 for 40 in the build instructions.

## Building

#### Custom install location

The command-line examples provided below will install everything into the default system location.

To specify a custom install location, add `-DCMAKE_INSTALL_PREFIX=/some/directory` to each of the CMake commands given below.

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

[llvm-cmake]: http://llvm.org/releases/4.0.0/docs/CMake.html

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
-Mextened             Allow source lines up to 132 characters
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

