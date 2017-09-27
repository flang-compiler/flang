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

########## Make rule for test ieee05e  ########

CWD   := $(shell pwd)
INVOKEE=runieee

ieee05e: ieee05e.$(OBJX)

ieee05e.$(OBJX):  $(SRC)/ieee05e.f90
	-$(RM) ieee05e.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	@echo $(CWD)/ieee05e.$(EXESUFFIX) > $(INVOKEE)
	chmod 744 $(INVOKEE)
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/ieee05e.f90 -o ieee05e.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) ieee05e.$(OBJX) check.$(OBJX) $(LIBS) -o ieee05e.$(EXESUFFIX)


ieee05e.run: ieee05e.$(OBJX)
	@echo ------------------------------------ executing test ieee05e
	$(shell ./$(INVOKEE) > ieee05e.res 2> ieee05e.err)
	@cat ieee05e.res
run: ieee05e.$(OBJX)
	@echo ------------------------------------ executing test ieee05e
	$(shell ./$(INVOKEE) > ieee05e.res 2> ieee05e.err)
	@cat ieee05e.res
build:	ieee05e.$(OBJX)
verify:	;

