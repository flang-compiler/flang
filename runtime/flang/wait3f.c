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

/*	wait3f.c - Implements LIB3F wait subprogram.  */

#ifndef _WIN32

#include <sys/types.h>
#include <sys/wait.h>
#include "ent3f.h"

/* The type of the wait system call argument differs between various
 * Linux flavaors and OSX
 */
#define WAIT_STAT int*

int ENT3F(WAIT, wait)(int *st) 
{
  WAIT_STAT wst = (WAIT_STAT)st;
  return wait(wst);
}

#endif /* !WINNT */
