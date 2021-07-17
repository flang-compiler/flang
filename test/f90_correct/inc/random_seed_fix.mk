#
# Copyright (c) 2019, Arm Ltd.  All rights reserved.
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

EXE=random_seed_fix.$(EXESUFFIX)

build:  $(SRC)/random_seed_fix.f90
	-$(RM) random_seed_fix.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	-$(RM) $(OBJ)
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	@echo ------------------------------------ building test $@
	$(FC) $(FFLAGS) $(LDFLAGS) $(SRC)/random_seed_fix.f90 check.$(OBJX) -o random_seed_fix.$(EXESUFFIX)

run:
	@echo ------------------------------------ executing test random_seed_fix
	random_seed_fix.$(EXESUFFIX)

verify: ;

random_seed_fix.run: run
