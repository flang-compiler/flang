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

########## Make rule for test mmul_misc  ########


mmul_misc: run
	

build:  $(SRC)/mmul_misc.f90
	-$(RM) mmul_misc.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/mmul_misc.f90 -o mmul_misc.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) mmul_misc.$(OBJX) check.$(OBJX) $(LIBS) -o mmul_misc.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test mmul_misc
	mmul_misc.$(EXESUFFIX)

verify: ;

mmul_misc.run: run

