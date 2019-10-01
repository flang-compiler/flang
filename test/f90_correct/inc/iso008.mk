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
########## Make rule for test iso008  ########


iso008: run
	

build:  $(SRC)/iso008.f90
	-$(RM) iso008.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/c_iso008.c -o c_iso008.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/iso008.f90 -o iso008.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) iso008.$(OBJX) c_iso008.$(OBJX) $(LIBS) -o iso008.$(EXESUFFIX)


run: 
	@echo ------------------------------------ executing test iso008
	iso008.$(EXESUFFIX)

verify: ;

