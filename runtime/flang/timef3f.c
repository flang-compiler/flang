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

/*	timef3f.c - Implements timef subprogram.  */

/* assumes the Unix times system call */
/* how do we do this for WINNT */
#include "ent3f.h"

#define _LIBC_LIMITS_H_
#ifndef _WIN32
#include <unistd.h>
#include <sys/times.h>
#endif
#include <sys/types.h>
#include <limits.h>

#ifndef CLK_TCK
#define CLK_TCK sysconf(_SC_CLK_TCK)
#endif

static clock_t start = 0;

double ENT3F(TIMEF, timef)(float *tarray)
{
  struct tms b;
  clock_t current;
  double duration;
  double inv_ticks = 1 / (double)CLK_TCK;

  times(&b);
  if (start == 0) {
    start = b.tms_utime + b.tms_stime;
    current = start;
  } else
    current = b.tms_utime + b.tms_stime;

  duration = ((double)(current - start)) * inv_ticks;
  return duration;
}

