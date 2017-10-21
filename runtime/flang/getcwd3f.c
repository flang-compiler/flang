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

/*	getcwd3f.c - Implements LIB3F getcwd subprogram.  */

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"
#include "utils3f.h"
#include "mpalloc.h"

#if defined(_WIN32)
#define GETCWDM _getcwd /* getcwd deprecated in Windows in VC 2005 */
#else
#define GETCWDM getcwd
#endif

extern char *GETCWDM();

int ENT3F(GETCWD, getcwd)(DCHAR(dir) DCLEN(dir))
{
  char *p;
  int i;

  p = GETCWDM(NULL, CLEN(dir) + 1);
  if (p) {
    __fcp_cstr(CADR(dir), CLEN(dir), p);
    _mp_free(p);
    i = 0;
  } else
    i = __io_errno();

  return i;
}
