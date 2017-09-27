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

########## Make rule for test fs22359.mk  ########


fs22359: run

build:  $(SRC)/fs22359.f90
	-$(RM) fs22359.$(OBJX) core *.d *.mod
	@echo ------------------------------------ building test $@
	-$(FC) $(FFLAGS) $(LDFLAGS) $(SRC)/fs22359.f90 $(LIBS) -o fs22359.$(EXESUFFIX)


run: 
	@echo ------------------------------------ executing test fs22359
	fs22359.$(EXESUFFIX)

verify: ;

fs22359.run: run

