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

/*	unpacktimeqq3f.c - Implements DFLIB packtimeqq subprogram.  */
#if defined(_WIN32)
#include <windows.h>
#endif
#include <string.h>
#include <stdlib.h>
#include "mpalloc.h"
/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

#if defined(_WIN32)
extern char *__fstr2cstr();
extern void __UnpackTime(unsigned int secsSince1970, ULARGE_INTEGER *fileTime);

void ENT3F(UNPACKTIMEQQ, unpacktimeqq)(unsigned int *timedate, int *year,
                                       int *month, int *day, int *hour,
                                       int *minute, int *second)
{
  SYSTEMTIME *sysTime;
  FILETIME *fileTime;
  ULARGE_INTEGER quadTime;

  fileTime = (FILETIME *)_mp_malloc(sizeof(FILETIME));
  sysTime = (SYSTEMTIME *)_mp_malloc(sizeof(SYSTEMTIME));
  __UnpackTime(*timedate, &quadTime);
  fileTime->dwLowDateTime = quadTime.u.LowPart;
  fileTime->dwHighDateTime = quadTime.u.HighPart;
  FileTimeToSystemTime(fileTime, sysTime);

  *year = sysTime->wYear;
  *month = sysTime->wMonth;
  *day = sysTime->wDay;
  *hour = sysTime->wHour;
  *minute = sysTime->wMinute;
  *second = sysTime->wSecond;

  _mp_free(fileTime);
  _mp_free(sysTime);
}
#else
void ENT3F(UNPACKTIMEQQ, unpacktimeqq)(int *timedate, int *year, int *month,
                                       int *day, int *hour, int *minute,
                                       int *second)
{
  fprintf(__io_stderr(), "unpacktimeqq() not implemented on this target\n");
}

#endif
