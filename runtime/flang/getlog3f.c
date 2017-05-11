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

/*	getlog3f.c - Implements LIB3F getlog subprogram.  */

#ifndef WINNT

#include "ent3f.h"
#include "utils3f.h"

extern char *getlogin();

void ENT3F(GETLOG, getlog)(DCHAR(nm) DCLEN(nm))
{
  char *p;

  p = getlogin();
  __fcp_cstr(CADR(nm), CLEN(nm), p);
}

#endif /* !WINNT */
