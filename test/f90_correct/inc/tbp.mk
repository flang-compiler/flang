#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#


########## Make rule to test type-bound procedures ########

fcheck.o check_mod.mod: $(SRC)/check_mod.f90
	-$(FC) -c $(FFLAGS) $(SRC)/check_mod.f90 -o fcheck.o

tbp.o:  $(SRC)/tbp.f90 check_mod.mod
	@echo ------------------------------------ building test $@
	-$(FC) -c $(FFLAGS) $(LDFLAGS) $(SRC)/tbp.f90 -o tbp.o

tbp: tbp.o fcheck.o
	-$(FC) $(FFLAGS) $(LDFLAGS) tbp.o fcheck.o $(LIBS) -o tbp

tbp.run: tbp
	@echo ------------------------------------ executing test tbp
	tbp
	-$(RM) test_m.mod

### TA Expected Targets ###

build: $(TEST)

.PHONY: run
run: $(TEST).run

verify: ; 

### End of Expected Targets ###
