/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/* clang-format off */

/*	besyn3f.c - Implements LIB3F besyn subprogram.  */

#include "ent3f.h"

#if defined(_WIN64)
#define yn _yn
#endif

extern double yn(int, double);

float ENT3F(BESYN, besyn)(int *n, float *x) { return (float)yn(*n, *x); }
