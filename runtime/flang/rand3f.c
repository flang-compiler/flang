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

/*	rand3f.c - Implements LIB3F rand subprogram.  */

#include "ent3f.h"

/* drand48 is not currently available on win64 */
#if defined(WIN64) || defined(WIN32)

#include <stdlib.h>

float ENT3F(RAND1, rand1)()
{
  float scale, base, fine;

  scale = RAND_MAX + 1.0;
  base = rand() / scale;
  fine = rand() / scale;
  return base + fine / scale;
}

float ENT3F(RAND2, rand2)(int *flag)
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

double ENT3F(RAND, rand)(void) { return drand48(); }

#endif
