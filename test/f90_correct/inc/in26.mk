#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#


########## Make rule for test in26  ########


in26: run
	

fcheck.$(OBJX): $(SRC)/check_mod.f90
	-$(FC) -c $(FFLAGS) $(SRC)/check_mod.f90 -o fcheck.$(OBJX)

build:  $(SRC)/in26.f90 $(SRC)/in26_expct.c fcheck.$(OBJX)
	-$(RM) in26.$(EXESUFFIX) in26.$(OBJX) in26_expct.$(OBJX)
	@echo ------------------------------------ building test $@
	-$(CC) -g -c $(CFLAGS) $(SRC)/in26_expct.c -o in26_expct.$(OBJX)
	-$(FC) -g -c $(FFLAGS) $(LDFLAGS) $(SRC)/in26.f90 -o in26.$(OBJX)
	-$(FC) -g $(FFLAGS) $(LDFLAGS) in26.$(OBJX) in26_expct.$(OBJX) fcheck.$(OBJX) $(LIBS) -o in26.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test in26
	in26.$(EXESUFFIX)

verify: ;

in26.run: run

