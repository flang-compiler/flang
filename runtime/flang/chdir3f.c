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

/*	chdir3f.c - Implements LIB3F chdir subprogram.  */

/* must include ent3f.h AFTER io3f.h */
#ifndef _WIN32
#include <unistd.h>
#endif
#include "io3f.h"
#include "ent3f.h"

#ifdef WIN32
#define chdir _chdir
#endif

extern char *__fstr2cstr();
extern void __cstr_free();

int ENT3F(CHDIR, chdir)(DCHAR(path) DCLEN(path))
{
  char *p;
  int i;

  p = __fstr2cstr(CADR(path), CLEN(path));
  if ((i = chdir(p)))
    i = __io_errno();
  __cstr_free(p);
  return i;
}
