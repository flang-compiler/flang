#
# Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
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

########## Make rule for test fs12  ########

# Skip checking for mcopy call if set to 1
SKIP_CALL_CHECK=0

# Determine call instruction used
# Note that llvm opt may optimize the code and may not all 3 as a call
# Update a checked to just check the present of of pgf90_mcopy
INSN=call
ifeq ($(findstring armv7l, $(UNAME)), armv7l)
    INSN=bl
endif
ifeq ($(findstring ppc64le, $(UNAME)), ppc64le)
    INSN=bl
endif

# Don't check call for LLVM compilers
ifeq ($(findstring pgf90-llvm, $(FC)), pgf90-llvm)
    SKIP_CALL_CHECK=1
endif
ifeq ($(findstring flang, $(FC)), flang)
    SKIP_CALL_CHECK=1
endif

fs12: run

build:  $(SRC)/fs12.f90
	-$(RM) fs12.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) $(SRC)/fs12.f90 -S 
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/fs12.f90 -o fs12.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) fs12.$(OBJX) check.$(OBJX) $(LIBS) -o fs12.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing modified test fs12
ifeq ($(SKIP_CALL_CHECK), 1)
	fs12.$(EXESUFFIX) 
else
	@if [ $(shell grep pgf90_mcopy fs12.s | grep -i $(INSN) | tr -s ' ' '\n' | grep -ci $(INSN)) = "3" ] ; \
	then \
	    fs12.$(EXESUFFIX) ; \
	else \
	    echo 'RESULT: FAIL - pgf90_mcopy not used' ; \
	    exit 1; \
	fi
endif

verify: ;

fs12.run: run
