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

########## Make rule for test bi01  ########


bi01: run
	

build:  $(SRC)/bi01.f90
	-$(RM) bi01.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(CC) -c $(CFLAGS) $(SRC)/bi01.c -o bi01_c.$(OBJX)
	-$(FC) -c $(FFLAGS) $(SRC)/bi01.f90 -o bi01_f.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) bi01_f.$(OBJX) bi01_c.$(OBJX) check.$(OBJX)  $(LIBS) -o bi01.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test bi01
	bi01.$(EXESUFFIX)

verify: ;

bi01.run: run

