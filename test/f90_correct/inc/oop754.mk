#
# Copyright (c) 2019, NVIDIA CORPORATION.  All rights reserved.
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
#
########## Make rule for test oop754  ########


oop754: run
	

build:  $(SRC)/oop754.f90
	-$(RM) oop754.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/oop754.f90 -o oop754.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) oop754.$(OBJX) check.$(OBJX) $(LIBS) -o oop754.$(EXESUFFIX)


run: 
	@echo ------------------------------------ executing test oop754
	oop754.$(EXESUFFIX)

verify: ;

