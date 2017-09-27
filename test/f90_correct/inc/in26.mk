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


########## Make rule for test in26  ########


in26: run
	

fcheck.$(OBJX): $(SRC)/check_mod.f90
	-$(FC) -c $(FFLAGS) $(SRC)/check_mod.f90 -o fcheck.$(OBJX)

build:  $(SRC)/in26.f90 $(SRC)/in26_expct.c fcheck.$(OBJX)
	-$(RM) in26.$(EXESUFFIX) in26.$(OBJX) in26_expct.$(OBJX)
	@echo ------------------------------------ building test $@
	-$(CC) -g -c $(CFLAGS) $(SRC)/in26_expct.c -o in26_expct.$(OBJX)
	-$(FC) -g -c $(FFLAGS) $(LDFLAGS) $(SRC)/in26.f90 -o in26.$(OBJX)
	-$(FC) -g $(FFLAGS) $(LDFLAGS) in26.$(OBJX) in26_expct.$(OBJX) fcheck.$(OBJX) $(LIBS) -o in26.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test in26
	in26.$(EXESUFFIX)

verify: ;

in26.run: run

