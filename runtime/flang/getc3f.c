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

/** \file
 * Implements LIB3F getc function. 
 */

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

extern FILE *__getfile3f();

int
ENT3F(GETC, getc)(DCHAR(ch) DCLEN(ch))
{
  FILE *f;
  int c;
  char *ch = CADR(ch);

  /* DON'T issue any error messages */

  f = __getfile3f(5);
  if (f) {
    c = fgetc(f);
    if (c == EOF) {
      if (__io_feof(f))
        return -1;
      return __io_errno();
    }
    *ch = c;
  }

  return 0;
}
