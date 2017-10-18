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

########## Make rule for test fs11  ########

# Determine call instruction used
INSN=call
ifeq ($(findstring aarch64, $(UNAME)), aarch64)
    INSN=bl
endif
ifeq ($(findstring ppc64le, $(UNAME)), ppc64le)
    INSN=bl
endif


fs11: run
	

build:  $(SRC)/fs11.f90
	-$(RM) fs11.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) $(SRC)/fs11.f90 -S 
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/fs11.f90 -o fs11.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) fs11.$(OBJX) check.$(OBJX) $(LIBS) -o fs11.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test fs11
	@if [ $(shell grep mmul fs11.s | grep $(INSN)|tr -s ' ' '\n' |grep -c $(INSN)) = "3" ] ; \
	then \
	    fs11.$(EXESUFFIX) ; \
	else \
	    echo 'RESULT: FAIL - mmul not used' ; \
	    exit 1; \
	fi

verify: ;

fs11.run: run

