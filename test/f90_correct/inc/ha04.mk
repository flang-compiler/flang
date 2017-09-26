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


########## Make rule for test ha04  ########
SHELL := /bin/bash

ha04: run


build:  $(SRC)/ha04.f90
	-$(RM) ha04.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/ha04.f90 -o ha04.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) ha04.$(OBJX) $(LIBS) -o ha04.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test ha04
	@ha04.$(EXESUFFIX)  >exitstr 2>&1; \
	stat=`echo $$?`; \
	cat exitstr ; \
	echo ------------------------------------- ; \
	exitstr=`cat exitstr | tail -n1 | tr -d " \n\r"`; \
	if [[ "$$stat" = "0" && "$$exitstr" = "byebye" ]] ; \
		 then echo "$$stat $$exitstr --   1 tests completed. 1 tests PASSED. 0 tests failed."; \
		 else echo "$$stat $$exitstr --   1 tests completed. 0 tests PASSED. 1 tests failed.";  fi;
	#@$(RM) exitstr;

verify: ;

ha04.run: run
