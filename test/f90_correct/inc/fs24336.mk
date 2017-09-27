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

########## Make rule for test fs24336.mk  ########


fs24336: run

build:  $(SRC)/fs24336_module.f90 $(SRC)/fs24336.f90
	-$(RM) fs24336_module.$(OBJX) fs24336.$(OBJX) core *.d *.mod
	@echo ------------------------------------ building test $@
	-$(FC) -v $(FFLAGS) $(LDFLAGS) $(SRC)/fs24336_module.f90 $(SRC)/fs24336.f90 $(LIBS) -o fs24336.$(EXESUFFIX)


run: 
	@echo ------------------------------------ executing test fs24336
	fs24336.$(EXESUFFIX)

verify: ;

fs24336.run: run

