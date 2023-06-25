# Build Flang from source in Docker

[Flang](https://github.com/flang-compiler/flang) (also known as "Classic Flang") is an out-of-tree Fortran compiler targeting LLVM.

This builds the Flang compiler in Ubuntu 22.04 Docker container, according to the instructions given on GitHub, plus OpenMP offload features for AMD and NVIDIA GPUs.

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

