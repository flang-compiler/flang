#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#

########## Make rule for test const_prop  ########


const_prop: run

build:  $(SRC)/const_prop.f90
	-$(RM) const_prop.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) -O2 $(LDFLAGS) $(SRC)/const_prop.f90 -o const_prop.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) const_prop.$(OBJX) check.$(OBJX) $(LIBS) -o const_prop.$(EXESUFFIX)


run:
	@echo ------------------------------------ executing test dt00
	const_prop.$(EXESUFFIX)

verify: ;

const_prop.run: run
