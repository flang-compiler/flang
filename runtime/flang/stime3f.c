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

/*	stime3f.c - Implements LIB3F stime subprogram.  */

#ifndef _WIN32

#include <time.h>
#include "io3f.h"
#include "ent3f.h"

int ENT3F(STIME, stime)(int *tp)
{
  int i;
  time_t t = *tp;

  if ((i = stime(&t)))
    i = __io_errno();

  return i;
}

#endif /* !WINNT */
