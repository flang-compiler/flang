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

/*	isatty3f.c - Implements LIB3F isatty subprogram.  */

#include "ent3f.h"

extern int __isatty3f();

int ENT3F(ISATTY, isatty)(int *lu)
{
  int u;

  if (__isatty3f(*lu))
    return -1; /* .true. */
  return 0;
}
