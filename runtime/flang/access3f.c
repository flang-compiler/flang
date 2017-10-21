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

/*	access3f.c - Implements LIB3F access subroutine.  */

/* must include ent3f.h AFTER io3f.h */
#ifndef _WIN32
#include <unistd.h>
#endif

#include "io3f.h"
#include "ent3f.h"

extern char *__fstr2cstr();
extern void __cstr_free();

int ENT3F(ACCESS, access)(DCHAR(fil), DCHAR(mode) DCLEN(fil) DCLEN(mode))
{
  char *nam;
  int i;
  int stat;
  int m;
  char *mode = CADR(mode);
  int mode_l = CLEN(mode);

  nam = __fstr2cstr(CADR(fil), CLEN(fil));
  m = 0;
  while (mode_l-- > 0) {
    switch (*mode) {
    case 'r':
      m |= 4;
      break;
    case 'w':
      m |= 2;
      break;
    case 'x':
      m |= 1;
      break;
    case ' ':
      break;
    default:
      fprintf(__io_stderr(), "Illegal access mode %c\n", *mode);
    }
    mode++;
  }
  if ((i = access(nam, m)) == 0)
    stat = 0;
  else if (i == -1)
    stat = __io_errno();
  else
    stat = -1;
  __cstr_free(nam);
  return stat;
}
