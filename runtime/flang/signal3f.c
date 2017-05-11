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

/*	signal3f.c - Implements LIB3F signal subprogram.  */

#include <signal.h>

#include "io3f.h"
#include "ent3f.h"

/*
extern void (*signal(int, void (*)(int)))(int);
*/

int ENT3F(SIGNAL, signal)(int *signum, void (*proc)(int), int *flag)
{
  void (*p)();

  if (*flag < 0)
    p = (void (*)())signal(*signum, proc);
  else
    p = (void (*)())signal(*signum, (void (*)())(long)*flag);
  if (p == (void (*)()) - 1)
    return -__io_errno();

  return 0;
}
