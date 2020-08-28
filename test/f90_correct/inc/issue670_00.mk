#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#

########## Make rule for test issue670_00  ########


issue670_00: run
	

build:  $(SRC)/issue670_00.f90
	-$(RM) issue670_00.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/issue670_00.f90 -o issue670_00.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) issue670_00.$(OBJX) check.$(OBJX) $(LIBS) -o issue670_00.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test issue670_00
	issue670_00.$(EXESUFFIX)

verify: ;

issue670_00.run: run

