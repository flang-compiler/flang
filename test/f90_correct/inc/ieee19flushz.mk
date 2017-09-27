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

########## Make rule for test ieee19flushz  ########

ieee19flushz: ieee19flushz.$(OBJX)
	

ieee19flushz.$(OBJX):  $(SRC)/ieee19flushz.f90
	-$(RM) ieee19flushz.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/ieee19flushz.f90 -o ieee19flushz.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) ieee19flushz.$(OBJX) check.$(OBJX) $(LIBS) -o ieee19flushz.$(EXESUFFIX)


ieee19flushz.run: ieee19flushz.$(OBJX)
	@echo ------------------------------------ executing test ieee19flushz
	ieee19flushz.$(EXESUFFIX)

verify: ;
build: ieee19flushz.$(OBJX) ;
run: ieee19flushz.run ;
