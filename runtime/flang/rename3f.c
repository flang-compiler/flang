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

/*	rename3f.c - Implements LIB3F rename subprogram.  */

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

extern char *__fstr2cstr();
extern void __cstr_free();

int ENT3F(RENAME, rename)(DCHAR(from), DCHAR(to) DCLEN(from) DCLEN(to))
{
  int i;

  char *old, *new;

  old = __fstr2cstr(CADR(from), CLEN(from));
  new = __fstr2cstr(CADR(to), CLEN(to));
  if ((i = rename(old, new)))
    i = __io_errno();
  __cstr_free(old);
  __cstr_free(new);

  return i;
}
