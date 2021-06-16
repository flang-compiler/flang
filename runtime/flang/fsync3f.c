/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/* clang-format off */

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"
#include "stdioInterf.h"

extern FILE *__getfile3f();

void ENT3F(FSYNC, fsync)(lu) int *lu;
{
  FILE *f;

  f = __getfile3f(*lu);
  if (f)
    #ifndef _WIN64
        fsync(__io_getfd(f));
    #else
        fflush(f);
    #endif
  return;
}
