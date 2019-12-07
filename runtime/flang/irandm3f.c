/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/* clang-format off */

/*	irandm3f.c - Implements LIB3F irandm subprogram.  */

#include "ent3f.h"

#ifdef WINNT

int ENT3F(IRANDM, irandm)(int *flag)
{
  if (*flag)
    srand(*flag);
  return rand();
}

#else

extern int lrand48();
extern void srand48();

int ENT3F(IRANDM, irandm)(int *flag)
{
  if (*flag)
    srand48(*flag);
  return lrand48();
}

#endif
