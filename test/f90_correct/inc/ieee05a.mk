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

########## Make rule for test ieee05a  ########

CWD   := $(shell pwd)
INVOKEE=runieee

ieee05a: ieee05a.$(OBJX)

ieee05a.$(OBJX):  $(SRC)/ieee05a.f90
	-$(RM) ieee05a.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	@echo $(CWD)/ieee05a.$(EXESUFFIX) > $(INVOKEE)
	chmod 744 $(INVOKEE)
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/ieee05a.f90 -o ieee05a.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) ieee05a.$(OBJX) check.$(OBJX) $(LIBS) -o ieee05a.$(EXESUFFIX)


ieee05a.run: ieee05a.$(OBJX)
	@echo ------------------------------------ executing test ieee05a
	$(shell ./$(INVOKEE) > ieee05a.res 2> ieee05a.err)
	@cat ieee05a.res

run: ieee05a.$(OBJX)
	@echo ------------------------------------ executing test ieee05a
	$(shell ./$(INVOKEE) > ieee05a.res 2> ieee05a.err)
	@cat ieee05a.res

build:	ieee05a.$(OBJX)
verify:	;
