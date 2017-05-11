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

/*	irandm3f.c - Implements LIB3F irandm subprogram.  */

#include "ent3f.h"

#ifdef WINNT

int ENT3F(IRANDM, irandm)(int *flag)
{
  if (*flag)
    srand(*flag);
  return rand();
}

#else

extern int lrand48();
extern void srand48();

int ENT3F(IRANDM, irandm)(int *flag)
{
  if (*flag)
    srand48(*flag);
  return lrand48();
}

#endif
