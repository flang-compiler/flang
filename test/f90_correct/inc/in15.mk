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


########## Make rule for test in15  ########


in15: run
	

fcheck.$(OBJX): $(SRC)/check_mod.f90
	-$(FC) -c $(FFLAGS) $(SRC)/check_mod.f90 -o fcheck.$(OBJX)

build:  $(SRC)/in15.f90 $(SRC)/in15_expct.c fcheck.$(OBJX)
	-$(RM) in15.$(EXESUFFIX) in15.$(OBJX) in15_expct.$(OBJX)
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/in15_expct.c -o in15_expct.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/in15.f90 -o in15.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) in15.$(OBJX) in15_expct.$(OBJX) fcheck.$(OBJX) $(LIBS) -o in15.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test in15
	in15.$(EXESUFFIX)

verify: ;

in15.run: run

