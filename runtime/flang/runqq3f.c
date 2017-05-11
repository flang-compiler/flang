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

/*	run3fqq.c - Implements DFLIB runqq subprogram.  */

#include <string.h>
#include "ent3f.h"
#include "mpalloc.h"

extern char *__fstr2cstr();
extern void __cstr_free();
extern void *_mp_malloc();

short ENT3F(RUNQQ, runqq)(DCHAR(fname), DCHAR(cline) DCLEN(fname) DCLEN(cline))
{
  char *fn;
  char *cl;
  char *m;
  short i;
  int len;

  fn = __fstr2cstr(CADR(fname), CLEN(fname));
  cl = __fstr2cstr(CADR(cline), CLEN(cline));
  len = strlen(fn) + strlen(cl) + 1;
  m = (char *)_mp_malloc((len + 1) * sizeof(char));
  m = strcpy(m, fn);
  m = strcat(m, " ");
  m = strcat(m, cl);
  i = system(m);
  _mp_free(m);
  __cstr_free(fn);
  __cstr_free(cl);
  return i;
}
