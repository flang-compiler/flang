#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#

########## Make rule for test qsin qtan qcos  ########


qtriangle: run
	

build:  $(SRC)/qtriangle.f08
	-$(RM) qtriangle.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/qtriangle.f08 -o qtriangle.$(OBJX)
	-$(FC)  $(FFLAGS) $(LDFLAGS) qtriangle.$(OBJX) check.$(OBJX) $(LIBS) -o qtriangle.$(EXESUFFIX)


run: 
	@echo ------------------------------------ executing test qtriangle 
	qtriangle.$(EXESUFFIX)

verify: ;


