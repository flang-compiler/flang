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
#    FC_FLAGS :=
#    -O2 -fp-model source -convert big_endian -assume byterecl -ftz
#    -traceback -assume realloc_lhs  -xAVX
#
FC_FLAGS := $(OPT)
FC_FLAGS += -Mnofma

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


ALL_OBJS := kernel_driver.o rrtmg_sw_rad.o kgen_utils.o shr_kind_mod.o rrtmg_sw_cldprmc.o rrsw_wvn.o rrsw_cld.o parrrsw.o rrsw_vsn.o

verify: 
	@(grep "verification.FAIL" $(TEST).rslt && echo "FAILED") || (grep "verification.PASS" $(TEST).rslt -q && echo PASSED)

run: build
	@mkdir rundir ; if [ ! -d data ] ; then ln -s $(SRC)/data data &&  echo "symlinked data directory: ln -s $(SRC)/data data"; fi; cd rundir; ../kernel.exe >> ../$(TEST).rslt 2>&1 || ( echo RUN FAILED: DID NOT EXIT 0)
# symlink data/ so it can be found in the directory made by lit
	 @echo ----------------------run-ouput-was----------
	 @cat $(TEST).rslt

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: $(SRC_DIR)/kernel_driver.f90 rrtmg_sw_rad.o kgen_utils.o shr_kind_mod.o rrtmg_sw_cldprmc.o rrsw_wvn.o rrsw_cld.o parrrsw.o rrsw_vsn.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_rad.o: $(SRC_DIR)/rrtmg_sw_rad.f90 kgen_utils.o rrtmg_sw_cldprmc.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: $(SRC_DIR)/shr_kind_mod.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_cldprmc.o: $(SRC_DIR)/rrtmg_sw_cldprmc.f90 kgen_utils.o shr_kind_mod.o parrrsw.o rrsw_vsn.o rrsw_wvn.o rrsw_cld.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_wvn.o: $(SRC_DIR)/rrsw_wvn.f90 kgen_utils.o parrrsw.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_cld.o: $(SRC_DIR)/rrsw_cld.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parrrsw.o: $(SRC_DIR)/parrrsw.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_vsn.o: $(SRC_DIR)/rrsw_vsn.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: $(SRC_DIR)/kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o *.rslt
