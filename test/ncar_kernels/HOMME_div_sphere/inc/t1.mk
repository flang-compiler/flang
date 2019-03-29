#
# Copyright (c) 2016-2019, NVIDIA CORPORATION.  All rights reserved.
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
# PGI
#
#FC := pgf95
#FFLAGS := -O3
#
# Intel
#
# FC := pgfortran
# FFLAGS := -O3 -mmic -qopt-report=5 -fp-model fast
# FFLAGS := -O3 -xCORE-AVX2 -qopt-report=5 -fp-model fast
# FFLAGS := -O3 -xAVX -qopt-report=5 -fp-model fast
#
# GFORTRAN
# 
# FC :=gfortran
# FFLAGS := -O3 -ffree-form -ffree-line-length-none -D__GFORTRAN__ -I./
# #
#
# Cray 
#
#  FC := ftn
#  FFLAGS := -O2
#

FC_FLAGS := $(OPT)

ALL_OBJS := kernel_divergence_sphere.o

all: build run verify

verify:
	@echo "nothing to be done for verify"

run:
	mkdir rundir; cd rundir; ../kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_divergence_sphere.o: $(SRC_DIR)/kernel_divergence_sphere.F90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f *.exe *.optrpt *.o *.oo *.mod
