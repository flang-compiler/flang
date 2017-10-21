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

/*	getfileinfoqq3f.c - Implements DFLIB getfileinfoqq subprogram.  */
#if defined(_WIN32)
#include <windows.h>
#endif
#include <string.h>
#include <stdlib.h>
/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

#if defined(_WIN32)
extern char *__fstr2cstr();
extern void __GetTimeToSecondsSince1970(ULARGE_INTEGER *fileTime,
                                        unsigned int *out);
extern int __GETFILEINFOQQ(DCHAR(ffiles), char *buffer,
                           int *handle DCLEN(ffiles));

int ENT3F(GETFILEINFOQQ, getfileinfoqq)(DCHAR(ffiles), char *buffer,
                                        int *handle DCLEN(ffiles))
{
  return __GETFILEINFOQQ(CADR(ffiles), buffer, handle, CLEN(ffiles));
}
#else
int ENT3F(GETFILEINFOQQ, getfileinfoqq)(DCHAR(ffiles), int *buffer,
                                        int *handle DCLEN(ffiles))
{
  fprintf(__io_stderr(), "getfileinfoqq() not implemented on this target\n");
  return 0;
}

#endif
