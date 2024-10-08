#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#

########## Make rule for test minexponent00  ########


minexponent00: run
	

build:  $(SRC)/minexponent00.f08
	-$(RM) minexponent00.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/minexponent00.f08 -Mpreprocess -o minexponent00.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) minexponent00.$(OBJX) check.$(OBJX) $(LIBS) -o minexponent00.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test minexponent00
	minexponent00.$(EXESUFFIX)

verify: ;

minexponent00.run: run

