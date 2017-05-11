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

/*	getenv3f.c - Implements LIB3F getenv subprogram.  */

#include "ent3f.h"
#include "utils3f.h"


extern char *getenv();
extern char *__fstr2cstr();
extern void __cstr_free();

void ENT3F(GETENV, getenv)(DCHAR(en), DCHAR(ev) DCLEN(en) DCLEN(ev))
{
  char *p, *q;
  char ch;

  q = __fstr2cstr(CADR(en), CLEN(en));
  p = getenv(q);
  __fcp_cstr(CADR(ev), CLEN(ev), p);
  __cstr_free(q);
}
