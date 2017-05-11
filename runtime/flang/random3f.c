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

/*	random3f.c - Implements LIB3F random subprogram.  */

#include "ent3f.h"

/* drand48, srand48 are not currently available on win64 */
#if defined(WIN64) || defined(WIN32)

#include <stdlib.h>

float ENT3F(RANDOM, random)(int *flag)
{
  float scale, base, fine;

  if (*flag)
    srand(*flag);
  scale = RAND_MAX + 1.0;
  base = rand() / scale;
  fine = rand() / scale;
  return base + fine / scale;
}

#else

extern double drand48();
extern void srand48();

float ENT3F(RANDOM, random)(int *flag)
{
  if (*flag)
    srand48(*flag);
  return (float)drand48();
}

#endif
