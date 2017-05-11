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

/*	renamefileqq3f.c - Implements DFLIB renamefileqq routine.  */

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

extern int rename();
extern char *__fstr2cstr();
extern void __cstr_free();

int ENT3F(RENAMEFILEQQ, renamefileqq)(DCHAR(from),
                                      DCHAR(to) DCLEN(from) DCLEN(to))
{
  int i;

  char *old, *new;

  old = __fstr2cstr(CADR(from), CLEN(from));
  new = __fstr2cstr(CADR(to), CLEN(to));
  i = rename(old, new);
  __cstr_free(old);
  __cstr_free(new);

  if (i == 0)  /* success */
    return -1; /* .true. */
  else         /* failure */
    return 0;  /* .false */
}
