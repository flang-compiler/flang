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

/*	ttynam3f.c - Implements LIB3F ttynam subprogram.  */

#ifndef _WIN32

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"
#include "utils3f.h"

extern char *ttyname();

/* ttynam is a character function */
void ENT3F(TTYNAM, ttynam)(DCHAR(nm) DCLEN(nm), int *lu)
{
  int u;
  char *p;

  switch (*lu) {
  case 0:
    u = 2;
    break;
  case 5:
    u = 0;
    break;
  case 6:
    u = 1;
    break;
  default:
    p = 0;
    goto sk;
  }
  p = ttyname(u);
sk:
  __fcp_cstr(CADR(nm), CLEN(nm), p);
  /*
  if (p) free(p);
  */

  return;
}

#endif /* !WINNT */
