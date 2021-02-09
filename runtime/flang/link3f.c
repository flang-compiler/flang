/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/* clang-format off */

/*	link3f.c - Implements LIB3F link subprogram.  */

#if !defined(_WIN32)

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

extern char *__fstr2cstr();
extern void __cstr_free();

int ENT3F(LINK, link)(DCHAR(n1), DCHAR(n2) DCLEN(n1) DCLEN(n2))
{
  char *p1, *p2;
  int i;

  p1 = __fstr2cstr(CADR(n1), CLEN(n1));
  p2 = __fstr2cstr(CADR(n2), CLEN(n2));

  if ((i = link(p1, p2)))
    i = __io_errno();

  __cstr_free(p1);
  __cstr_free(p2);

  return i;
}

#endif /* !_WIN32 */
