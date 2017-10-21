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

/*	mclock3f.c - Implements LIB3F mclock subprogram.  */
#include "ent3f.h"

/* assumes the Unix times system call */

#if   defined(_WIN32)

#include <time.h>

int ENT3F(MCLOCK, mclock)(void) { return clock(); }

#else
#include <sys/times.h>

int ENT3F(MCLOCK, mclock)(void)
{
  struct tms buffer;

  times(&buffer);
  return (buffer.tms_utime + buffer.tms_cutime + buffer.tms_cstime);
}

#endif
