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

########## Make rule for test ieee_denorm  ########


ieee_denorm: ieee_denorm.$(OBJX)

ieee_denorm.$(OBJX):  $(SRC)/ieee_denorm.f90
	-$(RM) ieee_denorm.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/ieee_denorm.f90 -o ieee_denorm.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) ieee_denorm.$(OBJX) check.$(OBJX) $(LIBS) -o ieee_denorm.$(EXESUFFIX)


ieee_denorm.run: ieee_denorm.$(OBJX)
	@echo ------------------------------------ executing test ieee_denorm
	ieee_denorm.$(EXESUFFIX)

verify: ;
build: ieee_denorm.$(OBJX) ;
run: ieee_denorm.run ;
