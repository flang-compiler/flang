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

########## Make rule for test ieee05c  ########

CWD   := $(shell pwd)
INVOKEE=runieee
FFLAGS += -Mpreprocess

ieee05c: ieee05c.$(OBJX)

ieee05c.$(OBJX):  $(SRC)/ieee05c.f90
	-$(RM) ieee05c.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	@echo $(CWD)/ieee05c.$(EXESUFFIX) > $(INVOKEE)
	chmod 744 $(INVOKEE)
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/ieee05c.f90 -o ieee05c.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) ieee05c.$(OBJX) check.$(OBJX) $(LIBS) -o ieee05c.$(EXESUFFIX)

ieee05c.run: ieee05c.$(OBJX)
	@echo ------------------------------------ executing test ieee05c
	$(shell ./$(INVOKEE) > ieee05c.res 2> ieee05c.err)
	@cat ieee05c.res

run: ieee05c.$(OBJX)
	@echo ------------------------------------ executing test ieee05c
	$(shell ./$(INVOKEE) > ieee05c.res 2> ieee05c.err)
	@cat ieee05c.res


build:	ieee05c.$(OBJX)
verify:	;
