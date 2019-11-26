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
########## Make rule for test pp73  ########


pp73: run
	

build:  $(SRC)/pp73.f90
	-$(RM) pp73.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/pp73.f90 -o pp73.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) pp73.$(OBJX) $(LIBS) -o pp73.$(EXESUFFIX)


run: 
	@echo ------------------------------------ executing test pp73
	pp73.$(EXESUFFIX)

verify: ;

