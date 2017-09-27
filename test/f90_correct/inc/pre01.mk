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

########## Make rule for test pre01  ########


pre01: run
FFLAGS += -Mpreprocess
	

build:  $(SRC)/pre01.f90
	-$(RM) pre01.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/pre01.f90 -o pre01.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) pre01.$(OBJX) check.$(OBJX) $(LIBS) -o pre01.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test pre01
	pre01.$(EXESUFFIX)

verify: ;

pre01.run: run

