#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#

########## Make rule for test qasin qacos qatan  ########


qatriangle: run
	

build:  $(SRC)/qatriangle.f08
	-$(RM) qatriangle.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/qatriangle.f08 -o qatriangle.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) qatriangle.$(OBJX) check.$(OBJX) $(LIBS) -o qatriangle.$(EXESUFFIX)


run: 
	@echo ------------------------------------ executing test qatriangle 
	qatriangle.$(EXESUFFIX)

verify: ;


