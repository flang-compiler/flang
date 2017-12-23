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

/*	etime3f.c - Implements LIB3F etime subprogram.  */

#include "ent3f.h"

/* assumes the Unix times system call */

/* Not implemented for WINNT */

#ifndef _WIN32
#include <unistd.h>
#include <sys/times.h>
#endif
#define _LIBC_LIMITS_H_
#include <sys/types.h>
#include <limits.h>

#ifndef CLK_TCK
#define CLK_TCK sysconf(_SC_CLK_TCK)
#endif

#ifndef _WIN32
float ENT3F(ETIME, etime)(float *tarray)
{
  struct tms b;
  float inv_ticks = 1 / (float)CLK_TCK;

  times(&b);
  tarray[0] = ((float)b.tms_utime) * inv_ticks;
  tarray[1] = ((float)b.tms_stime) * inv_ticks;
  return (tarray[0] + tarray[1]);
}

#else
#include <Windows.h>

float ENT3F(ETIME, etime)(float *tarray)
{
  FILETIME accum_user;
  FILETIME accum_sys;
  FILETIME time_create;
  FILETIME time_exit;

  GetProcessTimes( GetCurrentProcess(),
        &time_create, &time_exit, &accum_sys, &accum_user );

  tarray[0] = ((float)(convert_filetime(&accum_user)));
  tarray[1] = ((float)(convert_filetime(&accum_sys)));
  return (tarray[0] + tarray[1]);
}
#endif
