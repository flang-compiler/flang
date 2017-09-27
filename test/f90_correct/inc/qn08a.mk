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

########## Make rule for test qn08a  ########


qn08a: run
	

build:  $(SRC)/qn08a.f90
	-$(RM) qn08a.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/qn08a.f90 -o qn08a.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) qn08a.$(OBJX) check.$(OBJX) $(LIBS) -o qn08a.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test qn08a
	qn08a.$(EXESUFFIX)

verify: ;

qn08a.run: run

