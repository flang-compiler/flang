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

########## Make rule for test in29  ########


in29: run
	

fcheck.$(OBJX): $(SRC)/check_mod.f90
	-$(FC) -c $(FFLAGS) $(SRC)/check_mod.f90 -o fcheck.$(OBJX)

build:  $(SRC)/in29.f90 $(SRC)/in29_expct.c fcheck.$(OBJX)
	-$(RM) in29.$(EXESUFFIX) in29.$(OBJX) in29_expct.$(OBJX)
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/in29_expct.c -o in29_expct.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/in29.f90 -o in29.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) in29.$(OBJX) in29_expct.$(OBJX) fcheck.$(OBJX) $(LIBS) -o in29.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test in29
	in29.$(EXESUFFIX)

verify: ;

in29.run: run

