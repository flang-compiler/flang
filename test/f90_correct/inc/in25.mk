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


########## Make rule for test in25  ########


in25: run
	

fcheck.$(OBJX) check_mod.mod: $(SRC)/check_mod.f90
	-$(FC) -c $(FFLAGS) $(SRC)/check_mod.f90 -o fcheck.$(OBJX)

build:  $(SRC)/in25.f90 $(SRC)/in25_expct.c fcheck.$(OBJX) check_mod.mod
	-$(RM) in25.$(EXESUFFIX) in25.$(OBJX) in25_expct.$(OBJX)
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/in25_expct.c -o in25_expct.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/in25.f90 -o in25.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) in25.$(OBJX) in25_expct.$(OBJX) fcheck.$(OBJX) $(LIBS) -o in25.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test in25
	in25.$(EXESUFFIX)

verify: ;

in25.run: run

