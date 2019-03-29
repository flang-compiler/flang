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
#    FC_FLAGS := -fast -Mipa=fast,inline
# 
#  Intel default flags
#
#  FC_FLAGS :=  -assume byterecl -fp-model precise -ftz -O3 -g -openmp
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


ALL_OBJS := kernel_driver.o prim_advection_mod.o dimensions_mod.o kinds.o perf_utils.o perf_mod.o control_mod.o

verify: 
	@(grep "verification.FAIL" $(TEST).rslt && echo "FAILED") || (grep "verification.PASS" $(TEST).rslt -q && echo PASSED)

run: build
	@mkdir rundir ; if [ ! -d data ] ; then ln -s $(SRC)/data data &&  echo "symlinked data directory: ln -s $(SRC)/data data"; fi; cd rundir; ../kernel.exe >> ../$(TEST).rslt 2>&1 || ( echo RUN FAILED: DID NOT EXIT 0)
# symlink data/ so it can be found in the directory made by lit
	 @echo ----------------------run-ouput-was----------
	 @cat $(TEST).rslt

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: $(SRC_DIR)/kernel_driver.f90 prim_advection_mod.o dimensions_mod.o kinds.o perf_utils.o perf_mod.o control_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

prim_advection_mod.o: $(SRC_DIR)/prim_advection_mod.F90 kinds.o dimensions_mod.o perf_mod.o control_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

dimensions_mod.o: $(SRC_DIR)/dimensions_mod.F90 
	${FC} ${FC_FLAGS} -c -o $@ $<

kinds.o: $(SRC_DIR)/kinds.F90 
	${FC} ${FC_FLAGS} -c -o $@ $<

perf_utils.o: $(SRC_DIR)/perf_utils.F90 
	${FC} ${FC_FLAGS} -c -o $@ $<

perf_mod.o: $(SRC_DIR)/perf_mod.F90 perf_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

control_mod.o: $(SRC_DIR)/control_mod.F90 
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o *.rslt
