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

/*	fdate3f.c - Implements LIB3F fdate subprogram.  */

#include "ent3f.h"

#include <time.h>
#include "utils3f.h"

#if !defined(_WIN32)
WIN_MSVCRT_IMP char *WIN_CDECL ctime(const time_t *);
#endif

void ENT3F(FDATE, fdate)(DCHAR(str) DCLEN(str))
{
  char *str = CADR(str);
  int len = CLEN(str);
  time_t t;
  char *p;
  int i;

  t = time(0);
  p = ctime(&t);
  __fcp_cstr(str, len, p);
  for (i = len - 1; i >= 0; i--)
    if (str[i] == '\n') {
      str[i] = ' ';
      break;
    }

  return;
}
