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

/*	gerror3f.c - Implements LIB3F gerror subprogram.  */

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"
#include "utils3f.h"
#include "error.h"

#define Ftn_errmsg __fortio_errmsg

#if !defined(_WIN32)
extern char *strerror(); /* SVR4 only ? */
#endif

void ENT3F(GERROR, gerror)(DCHAR(str) DCLEN(str))
{
  char *p;

  p = strerror(__io_errno());
  __fcp_cstr(CADR(str), CLEN(str), p);
  return;
}

void ENT3F(GET_IOSTAT_MSG, get_iostat_msg)(int *ios, DCHAR(str) DCLEN(str))
{
  char *p;
  p = Ftn_errmsg(*ios);
  __fcp_cstr(CADR(str), CLEN(str), p);
}

/* for -Msecond_underscore */
void ENT3F(GET_IOSTAT_MSG_, get_iostat_msg_)(int *ios, DCHAR(str) DCLEN(str))
{
  char *p;
  p = Ftn_errmsg(*ios);
  __fcp_cstr(CADR(str), CLEN(str), p);
}
