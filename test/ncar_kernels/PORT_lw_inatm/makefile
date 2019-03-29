#
# Copyright (c) 2018-2019, NVIDIA CORPORATION.  All rights reserved.
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
# makefile for NCAR Kernel tests.


#TEST_DIR=./src
SRC=$(HOMEQA)
INCLUDES = $(HOMEQA)/inc
SRC_DIR=$(SRC)/src
FC=flang
OBJX=o
EXTRA_CFLAGS=
EXTRA_FFLAGS=
LD=$(FC)
RUN=
OPT=
ENDIAN=
FFLAGS=$(OPT)
LDFLAGS=$(EXTRA_LDFLAGS)
LIBS=$(EXTRA_LIBS)
KIEE=
CFLAGS=$(OPT) $(EXTRA_CFLAGS) 
EXE=out


RM=rm -f

TEST = t1
include $(INCLUDES)/$(TEST).mk
