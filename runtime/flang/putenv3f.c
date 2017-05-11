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
