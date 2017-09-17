Flang
=====

Flang is a Fortran compiler targeting LLVM.

See https://github.com/flang-compiler/flang

This builds the ``flang`` compiler on Fedora 26 according
to the instructions given on GitHub, but using the
distro provided llvm.

Image is uploaded to https://hub.docker.com/r/tdd20/flang/
(see also https://store.docker.com/community/images/tdd20/flang) 
so it is ready to use with:

    docker pull tdd20/flang

``flang`` and libraries are set up in the ``$PATH``.
