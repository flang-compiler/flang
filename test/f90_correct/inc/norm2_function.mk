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

EXE=norm2_function.$(EXESUFFIX)

build:  $(SRC)/norm2_function.F90
	-$(RM) norm2_function.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	-$(RM) $(OBJ)
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	@echo ------------------------------------ building test $@
	$(FC) $(FFLAGS) $(LDFLAGS) $(SRC)/norm2_function.F90 check.$(OBJX) -o norm2_function.$(EXESUFFIX)
	$(FC) $(FFLAGS) $(LDFLAGS) $(SRC)/norm2.F90 check.$(OBJX) -r8 -o norm2R8_function.$(EXESUFFIX)

run:
	@echo ------------------------------------ executing test norm2_function
	norm2_function.$(EXESUFFIX)
	norm2R8_function.$(EXESUFFIX)

verify: ;

norm2_function.run: run
