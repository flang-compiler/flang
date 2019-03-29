#
# Copyright (c) 2017-2019, NVIDIA CORPORATION.  All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#
#  PGI default flags
#
#    FC := pgf95
#    FC_FLAGS := -O3
# 
#  Intel default flags
#
#    FC := pgfortran
#    FFLAGS := -O3 -xCORE-AVX2 -qopt-report=5 -fp-model fast
#    FFLAGS := -O3 -align array64byte  -xCORE-AVX2 -qopt-report=5 -fp-model fast=2
#    FFLAGS := -O3  -xCORE-AVX2 -qopt-report=5 -fp-model fast=2
#    FFLAGS := -O3 -align array64byte -xAVX -fp-model fast=2
#    FFLAGS := -O3 -align array64byte -mmic -qopt-report=5 -fp-model fast=2
#    FFLAGS := -O3 -xAVX -qopt-report=5 -fp-model fast=2
#
# GFORTRAN
# 
#    FC :=gfortran
#    FFLAGS := -O3 -ffree-form -ffree-line-length-none -D__GFORTRAN__ -I./
#
#
# Makefile for KGEN-generated kernel
FC_FLAGS := $(OPT)

ifeq ("$(FC)", "pgf90")
endif
ifeq ("$(FC)", "pgfortran")
endif
ifeq ("$(FC)", "flang")
endif
ifeq ("$(FC)", "gfortran")
endif
ifeq ("$(FC)", "ifort")
endif
ifeq ("$(FC)", "xlf")
endif



ALL_OBJS := kernel_gradient_sphere.o

verify: 
	@echo "nothing to be done for verify"

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_gradient_sphere.o: $(SRC_DIR)/kernel_gradient_sphere.F90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o *.rslt
