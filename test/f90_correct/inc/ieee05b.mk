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

########## Make rule for test ieee05b  ########

CWD   := $(shell pwd)
INVOKEE=runieee

ieee05b: ieee05b.$(OBJX)
	

ieee05b.$(OBJX):  $(SRC)/ieee05b.f90
	-$(RM) ieee05b.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	@echo $(CWD)/ieee05b.$(EXESUFFIX) > $(INVOKEE)
	chmod 744 $(INVOKEE)
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/ieee05b.f90 -o ieee05b.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) ieee05b.$(OBJX) check.$(OBJX) $(LIBS) -o ieee05b.$(EXESUFFIX)


ieee05b.run: ieee05b.$(OBJX)
	@echo ------------------------------------ executing test ieee05b
	$(shell ./$(INVOKEE) > ieee05b.res 2> ieee05b.err)
	@cat ieee05b.res

run: ieee05b.$(OBJX)
	@echo ------------------------------------ executing test ieee05b
	$(shell ./$(INVOKEE) > ieee05b.res 2> ieee05b.err)
	@cat ieee05b.res
build:	ieee05b.$(OBJX)
verify:	;
