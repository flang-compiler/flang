/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/* clang-format off */

/*	stime3f.c - Implements LIB3F stime subprogram.  */

#ifndef WINNT

#include <time.h>
#include "io3f.h"
#include "ent3f.h"

int ENT3F(STIME, stime)(int *tp)
{
#ifdef TARGET_OSX
  /* stime() doesn't exist on maxOS and is root-only anyway */
  return EPERM;
#else
  int i;
  struct timespec ts = {0};

  ts.tv_sec = *tp;
  if ((i = clock_settime(CLOCK_REALTIME, &ts)))
    i = __io_errno();

  return i;
#endif
}

#endif /* !WINNT */
