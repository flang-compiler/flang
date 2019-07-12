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

EXE1=fix_len_returning_i8.$(EXESUFFIX)
EXE2=fix_len_returning_i8_di8.$(EXESUFFIX)

build:  $(SRC)/fix_len_returning_i8.f90
	-$(RM) fix_len_returning_i8.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	-$(RM) $(OBJ)
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	@echo ------------------------------------ building test $@
	$(FC) $(FFLAGS) $(LDFLAGS) $(SRC)/fix_len_returning_i8.f90 check.$(OBJX) -o $(EXE1)
	$(FC) $(FFLAGS) -fdefault-integer-8 $(LDFLAGS) $(SRC)/fix_len_returning_i8.f90 check.$(OBJX) -o $(EXE2)

run:
	@echo ------------------------------------ executing test $(EXE1)
	$(EXE1)
	@echo ------------------------------------ executing test $(EXE2)
	$(EXE2)

verify: ;

fix_len_returning_i8.run: run
