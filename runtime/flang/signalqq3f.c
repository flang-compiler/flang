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

/*	signalqq3f.c - Implements DFLIB signalqq subprogram.  */

#include <signal.h>

#include "io3f.h"
#include "ent3f.h"

#if defined(_WIN32) || !defined(_WIN32)

#define LONGINTSIZE unsigned long long

LONGINTSIZE
ENT3F(SIGNALQQ, signalqq)(short *signum, void (*proc)(int))
{
  void (*p)();

  p = (void (*)())signal(*signum, proc);
  if (p == (void (*)()) - 1)
    return -1;
  else
    return (LONGINTSIZE)p;
}

#endif /* WIN64 or !WINNT */
