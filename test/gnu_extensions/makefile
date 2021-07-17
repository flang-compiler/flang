# Copyright (c) 2018, Advanced Micro Devices, Inc. All rights reserved.
# Date of Modification: September 2019


BASE_DIR=$(HOMEQA)
SRC=$(BASE_DIR)/src
SRC2=$(BASE_DIR)/src/src  # For regression_cpu
FC=flang
CC=clang
CXX=clang++
OBJX=o
EXESUFFIX=out
LD=$(FC)
OPT=-O
FFLAGS=-I$(SRC) $(OPT) $(KIEE) $(EXTRA_FFLAGS) $(EXTRA_HFLAGS)
LDFLAGS=$(EXTRA_LDFLAGS)
LIBS=$(EXTRA_LIBS)
CFLAGS=$(EXTRA_CFLAGS)
TEST=
COMP_CHECK=python $(HOMEQA)/../tools/check_compilation.py

RM=rm -f
CP=cp -f
UNAME := $(shell uname -a)

INCLUDES=$(BASE_DIR)/inc

check: check.$(OBJX)

check.$(OBJX) : $(SRC)/check.c
	-$(CC) -c $(CFLAGS) $(SRC)/check.c -o check.$(OBJX)

clean.run: clean.$(OBJX)
	a.out

clean:
	-$(RM) a.out *.$(OBJX) *.mod *.qdbg core

run.run: run.$(OBJX)
	a.out

include $(INCLUDES)/$(TEST).mk
