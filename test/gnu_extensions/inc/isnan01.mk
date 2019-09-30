# Copyright (c) 2018, Advanced Micro Devices, Inc. All rights reserved.
# Date of Modification: September 2019

########## Make rule for test isnan01  ########


isnan01: isnan01.run

isnan01.$(OBJX):  $(SRC)/isnan01.f90
	-$(RM) isnan01.$(EXESUFFIX) core *.d *.mod FOR*.DAT FTN* ftn* fort.*
	@echo ------------------------------------ building test $@
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/isnan01.f90 -o isnan01.$(OBJX)
	-$(FC) $(FFLAGS) $(LDFLAGS) isnan01.$(OBJX) check.$(OBJX) $(LIBS) -o isnan01.$(EXESUFFIX)


isnan01.run: isnan01.$(OBJX)
	@echo ------------------------------------ executing test isnan01
	isnan01.$(EXESUFFIX)

build:	isnan01.$(OBJX)

verify:	;

run:	 isnan01.$(OBJX)
	@echo ------------------------------------ executing test isnan01
	isnan01.$(EXESUFFIX)
