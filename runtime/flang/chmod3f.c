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

/*	chmod3f.c - Implements LIB3F chmod subprogram.  */

/* must include ent3f.h AFTER io3f.h */
/* for chmod */
#include <sys/types.h>
#include <sys/stat.h>

#include "io3f.h"
#include "ent3f.h"

extern char *__fstr2cstr();
extern void __cstr_free();

int ENT3F(CHMOD, chmod)(DCHAR(nam), int *mode DCLEN(nam))
{
  char *p;
  int i;

  p = __fstr2cstr(CADR(nam), CLEN(nam));
  if ((i = chmod(p, *mode)))
    i = __io_errno();
  __cstr_free(p);
  return i;
}
