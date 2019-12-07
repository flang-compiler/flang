/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/* clang-format off */

/*	drandm3f.c - Implements LIB3F drandm subprogram.  */

#include "ent3f.h"

/* drand48, srand48 are not currently available on win64 */
#if defined(WIN64) || defined(WIN32)

#include <stdlib.h>

double ENT3F(DRANDM, drandm)(int *flag)
{
  double scale, base, fine;

  if (*flag)
    srand(*flag);
  scale = RAND_MAX + 1.0;
  base = rand() / scale;
  fine = rand() / scale;
  return base + fine / scale;
}

double ENT3F(DRAND, drand)(int *flag)
{
  return drandm_(flag);
}

#else

extern double drand48();
extern void srand48();

double ENT3F(DRANDM, drandm)(int *flag)
{
  if (*flag)
    srand48(*flag);
  return drand48();
}

#endif
