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

/*	setfileaccessqq3f.c - Implements DFLIB setfileaccessqq subprogram.  */
#if defined(WIN64) || defined(WIN32)
#include <windows.h>
#endif
#include <string.h>
#include <stdlib.h>
/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

#define FILE$FIRST -1
#define FILE$LAST -2
#define FILE$ERROR -3
#define FILE$CURTIME -1

#if defined(WIN64) || defined(WIN32)
extern char *__fstr2cstr();

int ENT3F(SETFILEACCESSQQ, setfileaccessqq)(DCHAR(ffile),
                                            int *access DCLEN(ffile))
{

  int success;
  char *fileName;

  fileName = __fstr2cstr(CADR(ffile), CLEN(ffile));
  if (!fileName)
    return 0;
  success = SetFileAttributes(fileName, *access);
  __cstr_free(fileName);
  return success ? -1 : 0;
}
#else
int ENT3F(SETFILEACCESSQQ, setfileaccessqq)(DCHAR(ffiles),
                                            int *access DCLEN(ffiles))
{
  fprintf(__io_stderr(),
          "setfileaccessqq() not implemented on this target\n");
  return 0;
}

#endif
