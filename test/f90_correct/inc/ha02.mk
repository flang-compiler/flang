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


########## Make rule for test ha02  ########
SHELL := /bin/bash

ha02: run


build:  $(SRC)/ha02.f90
	-$(RM) ha02.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/ha02.f90 -o ha02.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) ha02.$(OBJX) $(LIBS) -o ha02.$(EXESUFFIX)

#
# NOTE: Cygwin will detect the high-bit (sign bit) set and return a value 
# of 127.  We capture this case for Cygwin terminals in the conditional below.
#
run:
	@echo ------------------------------------ executing test ha02
	@ha02.$(EXESUFFIX) 1>exitstr 2>&1; \
	stat=`echo $$?`; \
	cat exitstr ; \
	echo ------------------------------------- ; \
	exitstr=`cat exitstr | tail -n 1 | tr -d " \n\r"`; \
	os=`uname -o`; \
	if [[ "$$os" = "Cygwin" && "$$stat" = "127" && "$$exitstr" = "-42" ]] ; \
		 then echo "$$stat $$exitstr -- 1 tests completed. 1 tests PASSED. 0 tests failed."; \
	elif [[ "$$stat" = "214" && "$$exitstr" = "-42" ]] ; \
		 then echo "$$stat $$exitstr -- 1 tests completed. 1 tests PASSED. 0 tests failed."; \
	else \
		echo "$$stat $$exitstr -- 1 tests completed. 0 tests PASSED. 1 tests failed."; \
	fi;
	@$(RM) exitstr;

verify: ;

ha02.run: run
