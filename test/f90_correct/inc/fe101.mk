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

########## Make rule for test fe101  ########


fe101: fe101.run
	

fe101.$(OBJX):  $(SRC)/fe101.f90
	-$(RM) fe101.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/fe101.f90 -o fe101.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) fe101.$(OBJX) check.$(OBJX) $(LIBS) -o fe101.$(EXESUFFIX)


fe101.run: fe101.$(OBJX)
	@echo ------------------------------------ executing test fe101
	fe101.$(EXESUFFIX)

build:	fe101.$(OBJX)

verify:	;

run:	 fe101.$(OBJX)
	@echo ------------------------------------ executing test fe101
	fe101.$(EXESUFFIX)
