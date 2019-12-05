/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/* clang-format off */

/*	putenv3f.c - Implements LIB3F putenv subprogram.  */

#include "ent3f.h"

extern char *__fstr2cstr();
extern void __cstr_free();

extern int putenv();

int ENT3F(PUTENV, putenv)(DCHAR(str) DCLEN(str))
{
  int i;
  char *p;

  p = __fstr2cstr(CADR(str), CLEN(str));
  i = putenv(p);
  /* note - putenv stashes the pointer rather than copying the
   *        value, so can't free p.
  __cstr_free(p);
   */

  return i;
}
