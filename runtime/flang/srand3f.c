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

/*	srand3f.c - Implements LIB3F srand subprogram.  */

#include <stdlib.h>
#include "ent3f.h"

/* srand48 is not currently available on win64 */
#if defined(_WIN32)

void ENT3F(SRAND1, srand1)(int *iseed) { srand(*iseed); }

void ENT3F(SRAND2, srand2)(float *rseed)
{
  int iseed;
  iseed = (int)(*rseed);
  srand(iseed);
}

#else

void ENT3F(SRAND, srand)(int *iseed) { srand48(*iseed); }

#endif
