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

########## Make rule for test ieee05d  ########

CWD   := $(shell pwd)
INVOKEE=runieee

ieee05d: ieee05d.$(OBJX)
	
ieee05d.$(OBJX):  $(SRC)/ieee05d.f90
	-$(RM) ieee05d.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	@echo $(CWD)/ieee05d.$(EXESUFFIX) > $(INVOKEE)
	chmod 744 $(INVOKEE)
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/ieee05d.f90 -o ieee05d.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) ieee05d.$(OBJX) check.$(OBJX) $(LIBS) -o ieee05d.$(EXESUFFIX)


ieee05d.run: ieee05d.$(OBJX)
	@echo ------------------------------------ executing test ieee05d
	$(shell ./$(INVOKEE) > ieee05d.res 2> ieee05d.err)
	@cat ieee05d.res

run: ieee05d.$(OBJX)
	@echo ------------------------------------ executing test ieee05d
	$(shell ./$(INVOKEE) > ieee05d.res 2> ieee05d.err)
	@cat ieee05d.res
build:	ieee05d.$(OBJX)
verify:	;
