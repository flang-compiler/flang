/*
 * Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

/* clang-format off */

/*	link3f.c - Implements LIB3F link subprogram.  */

#ifndef WINNT

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

#endif /* !WINNT */
