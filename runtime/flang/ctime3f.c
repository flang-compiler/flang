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

/*	ctime3f.c - Implements LIB3F ctime subprogram.  */

#include "ent3f.h"
#include "utils3f.h"

extern char *ctime(long *);

/* ctime is a character function */

static void
ctime_c(char *tm, int tml, long stime)
{
  char *p;
  int i;

  p = ctime(&stime); /* ctime arg is 'pointer to' */
  __fcp_cstr(tm, tml, p);
  for (i = tml - 1; i >= 0; i--)
    if (tm[i] == '\n') {
      tm[i] = ' ';
      break;
    }

  return;
}

void ENT3F(CTIME, ctime)(DCHAR(tm) DCLEN(tm), int *stime)
{
  ctime_c(CADR(tm), CLEN(tm), (long)(*stime));
}

void ENT3F(CTIME8, ctime8)(DCHAR(tm) DCLEN(tm), long long *stime)
{
  ctime_c(CADR(tm), CLEN(tm), (long)*stime);
}
