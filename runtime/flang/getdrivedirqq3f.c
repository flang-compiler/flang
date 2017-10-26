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

/*	getdrivedirqq3f.c - Implements DFLIB getdrivedirqq subprogram.  */

#include <string.h>
/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"
#include "utils3f.h"
#include "mpalloc.h"

#if defined(WIN64) || defined(WIN32)
#define GETCWDM _getcwd /* getcwd deprecated in Windows in VC 2005 */
#else
#define GETCWDM getcwd
#endif

extern char *GETCWDM();
extern char *__fstr2cstr();

int ENT3F(GETDRIVEDIRQQ, getdrivedirqq)(DCHAR(dir) DCLEN(dir))
{
  char *p, *q;
  int i, l1, l2;

  q = __fstr2cstr(CADR(dir), CLEN(dir));
  l1 = CLEN(dir) + 1;
  if (strlen(q) + 1 < l1)
    l1 = strlen(q);
  __cstr_free(q);
  p = GETCWDM(NULL, l1);
  if (p) {
    __fcp_cstr(CADR(dir), CLEN(dir), p);
    i = 0;
    l2 = strlen(p);
    if (l2 > CLEN(dir))
      l2 = 0;
    _mp_free(p);
  } else {
    i = __io_errno();
    l2 = 0;
  }

  return l2;
}
