Flang
=====

Flang is a Fortran compiler targeting LLVM.

Visit the flang wiki for more information:

https://github.com/flang-compiler/flang/wiki

We have mailing lists for announcements and developers.
Here's the link with the sign-up information:

http://lists.flang-compiler.org/mailman/listinfo

We have a flang-compiler channel on Slack.  Slack is invitation only but anyone can join.  Here's the link:

https://join.slack.com/t/flang-compiler/shared_invite/MjExOTEyMzQ3MjIxLTE0OTk4NzQyNzUtODQzZWEyMjkwYw


## Building Flang

Instructions for building Flang can be found on the Flang wiki:
https://github.com/flang-compiler/flang/wiki/Building-Flang


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

