#
# Copyright (c) 2016, Arm Ltd.  All rights reserved.
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

EXE=trailz.$(EXESUFFIX)

build:  $(SRC)/trailz.f90
	-$(RM) trailz.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	-$(RM) $(OBJ)
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	@echo ------------------------------------ building test $@
	$(FC) $(FFLAGS) $(LDFLAGS) $(SRC)/trailz.f90 check.$(OBJX) -o trailz.$(EXESUFFIX)

run:
	@echo ------------------------------------ executing test trailz
	trailz.$(EXESUFFIX)

verify: ;

trailz.run: run
