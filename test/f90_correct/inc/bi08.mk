# Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

########## Make rule for test bi08  ########

bi08: run
	
build:  $(SRC)/bi08.f90 $(SRC)/bi08c.c
	-$(RM) bi08.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/bi08c.c -o bi08c.$(OBJX)
	-$(FC) -c $(FFLAGS) $(SRC)/bi08.f90 -o bi08.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) bi08.$(OBJX) bi08c.$(OBJX) $(LIBS) -o bi08.$(EXESUFFIX)

run:
	@echo ------------------------------------ executing test bi08
	bi08.$(EXESUFFIX)

verify: ;

bi08.run: run

