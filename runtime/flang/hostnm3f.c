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

/*	hostnm3f.c - Implements LIB3F hostnm subprogram.  */

#ifndef _WIN32

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

extern int gethostname();

int ENT3F(HOSTNM, hostnm)(DCHAR(nm) DCLEN(nm))
{
  char *nm = CADR(nm);
  int len = CLEN(nm);
  int i;

  i = gethostname(nm, len);
  if (i < 0)
    i = __io_errno();
  else {
    /* note: last char stored is null character; gethostname() does
     *       not return the length of the name
     */
    for (i = 0; i < len; i++)
      if (nm[i] == '\0')
        break;
    /* i is position of null character, or len if not found */
    while (i < len) {
      nm[i] = ' ';
      i++;
    }
    i = 0;
  }
  return i;
}

#endif /* !WINNT */
