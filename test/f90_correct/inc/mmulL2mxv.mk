#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#

########## Make rule for test mmulL2mxv  ########


mmulL2mxv: run
	

build:  $(SRC)/mmulL2mxv.f90
	-$(RM) mmulL2mxv.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/mmulL2mxv.f90 -o mmulL2mxv.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) mmulL2mxv.$(OBJX) check.$(OBJX) $(LIBS) -o mmulL2mxv.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test mmulL2mxv
	mmulL2mxv.$(EXESUFFIX)

verify: ;

mmulL2mxv.run: run

