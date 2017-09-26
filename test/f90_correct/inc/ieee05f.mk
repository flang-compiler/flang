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

########## Make rule for test ieee05f  ########

CWD   := $(shell pwd)
INVOKEE=runieee

ieee05f: ieee05f.$(OBJX)

ieee05f.$(OBJX):  $(SRC)/ieee05f.f90
	-$(RM) ieee05f.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	@echo $(CWD)/ieee05f.$(EXESUFFIX) > $(INVOKEE)
	chmod 744 $(INVOKEE)
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/ieee05f.f90 -o ieee05f.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) ieee05f.$(OBJX) check.$(OBJX) $(LIBS) -o ieee05f.$(EXESUFFIX)


ieee05f.run: ieee05f.$(OBJX)
	@echo ------------------------------------ executing test ieee05f
	$(shell ./$(INVOKEE) > ieee05f.res 2> ieee05f.err)
	@cat ieee05f.res
run: ieee05f.$(OBJX)
	@echo ------------------------------------ executing test ieee05f
	$(shell ./$(INVOKEE) > ieee05f.res 2> ieee05f.err)
	@cat ieee05f.res

build:	ieee05f.$(OBJX)
verify:	;

