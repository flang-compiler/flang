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

#include "ent3f.h"

static int
_isnanf(volatile float x)
{
  union {
    float x;
    struct {
      unsigned int m : 23;
      unsigned int e : 8;
      unsigned int s : 1;
    } f;
  } u;

  u.x = x;
  return (u.f.e == 255 && u.f.m != 0);
}

int ENT3F(ISNANF, isnanf)(float *x)
{
  if (_isnanf(*x))
    return -1; /* .true. */
  return 0;
}
