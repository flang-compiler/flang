# Build Flang from source in Docker

[Flang](https://github.com/flang-compiler/flang) is a Fortran compiler targeting LLVM.

This builds the ``flang`` compiler on Ubuntu 22.04, according to the instructions given on GitHub.

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

## Usage

