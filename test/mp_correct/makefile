#
# Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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



BASE_DIR=$(HOMEQA)
SRC=$(BASE_DIR)/src
SRC2=$(BASE_DIR)/src/src  # For regression_cpu
FC=flang
F90=flang
CC=clang
CXX=clang++
OBJX=o
EXESUFFIX=out
LD=$(FC)
OPT=-O
MPFLAGS=-fopenmp
CMNFLAGS=-c -I$(SRC) $(OPT) $(MPFLAGS)
FFLAGS=$(CMNFLAGS) $(KIEE) $(EXTRA_FFLAGS) $(EXTRA_HFLAGS)
LDFLAGS=$(EXTRA_LDFLAGS) $(OPT) $(MPFLAGS)
LIBS=$(EXTRA_LIBS)
CFLAGS=$(CMNFLAGS) $(EXTRA_CFLAGS)
TEST=

RUN=
RUN1=OMP_NUM_THREADS=1
RUN2=OMP_NUM_THREADS=2
RUN4=OMP_NUM_THREADS=4
RUN12=OMP_NUM_THREADS=12
RUN65=OMP_NUM_THREADS=65

RM=rm -f
CP=cp -f

INCLUDES=$(BASE_DIR)/inc

check: check.$(OBJX)

check.$(OBJX) : $(SRC)/check.c
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)

clean.run: clean.$(OBJX)
	a.out

clean:
	-$(RM) a.out *.$(OBJX) *.mod *.qdbg core

run.run: run.$(OBJX)
	a.out

include $(INCLUDES)/$(TEST).mk

verify: ;
