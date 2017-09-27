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

########## Make rule for test pp03  ########


pp03: run
	

build:  $(SRC)/pp03.f90
	-$(RM) pp03.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/pp03.f90 -o pp03.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) pp03.$(OBJX) check.$(OBJX) $(LIBS) -o pp03.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test pp03
	pp03.$(EXESUFFIX)

verify: ;

pp03.run: run

