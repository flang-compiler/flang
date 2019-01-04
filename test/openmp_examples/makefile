#
# Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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
SRC_DIR=$(BASE_DIR)/openmp_examples
FC=flang
CC=clang
CXX=clang++
OBJX=o
EXESUFFIX=out
EXE=out


EXTRA_CFLAGS=
EXTRA_FFLAGS=
LD=$(FC)
RUN=
OPT=-O
MPFLAGS=-mp
CMNFLAGS=-c -I$(SRC) $(OPT) $(MPFLAGS)
FFLAGS=$(CMNFLAGS) $(KIEE) $(EXTRA_FFLAGS) $(EXTRA_HFLAGS)
LDFLAGS=$(EXTRA_LDFLAGS) $(OPT) $(MPFLAGS)
LIBS=$(EXTRA_LIBS)
CFLAGS=$(CMNFLAGS) $(EXTRA_CFLAGS)
TEST = 
RM=rm -f
CP=cp -f
RUN=
RUN1=OMP_NUM_THREADS=1
RUN2=OMP_NUM_THREADS=2
RUN4=OMP_NUM_THREADS=4
RUN12=OMP_NUM_THREADS=12
RUN65=OMP_NUM_THREADS=65
COMP_CHECK = python $(BASE_DIR)/../tools/check_compilation.py

INCLUDES = $(SRC)/sources/inc
include $(INCLUDES)/$(TEST).mk

