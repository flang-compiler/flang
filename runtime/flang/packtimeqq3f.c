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

/*	packtimeqq3f.c - Implements DFLIB packtimeqq subprogram.  */
#if defined(WIN64) || defined(WIN32)
#include <windows.h>
#endif
#include <string.h>
#include <stdlib.h>
/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

#if defined(WIN64) || defined(WIN32)
extern char *__fstr2cstr();
extern void __GetTimeToSecondsSince1970(ULARGE_INTEGER *fileTime,
                                        unsigned int *out);

void ENT3F(PACKTIMEQQ, packtimeqq)(unsigned int *timedate, int *year,
                                   int *month, int *day, int *hour, int *minute,
                                   int *second)
{
  SYSTEMTIME *sysTime;
  FILETIME *fileTime;
  ULARGE_INTEGER quadTime;
  int i;

  fileTime = (FILETIME *)_mp_malloc(sizeof(FILETIME));
  sysTime = (SYSTEMTIME *)_mp_malloc(sizeof(SYSTEMTIME));
  sysTime->wYear = *year;
  sysTime->wMonth = *month;
  sysTime->wDayOfWeek = 0;
  sysTime->wDay = *day;
  sysTime->wHour = *hour;
  sysTime->wMinute = *minute;
  sysTime->wSecond = *second;
  sysTime->wMilliseconds = 0;

  i = SystemTimeToFileTime(sysTime, fileTime);
  quadTime.u.LowPart = fileTime->dwLowDateTime;
  quadTime.u.HighPart = fileTime->dwHighDateTime;

  __GetTimeToSecondsSince1970(&quadTime, timedate);

  _mp_free(fileTime);
  _mp_free(sysTime);
}
#else
void ENT3F(PACKTIMEQQ, packtimeqq)(int *timedate, int *year, int *month,
                                   int *day, int *hour, int *minute,
                                   int *second)
{
  fprintf(__io_stderr(), "packtimeqq() not implemented on this target\n");
}

#endif
