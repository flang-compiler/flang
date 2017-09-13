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

#include "mthdecls.h"

/* should handle 0**0 w/ exception */
float
__mth_i_rpowk(float x, long long i)
{
  long long k;
  float f;

  f = 1;
  k = i;
  if (k < 0)
    k = -k;
  for (;;) {
    if (k & 1)
      f *= x;
    k >>= 1;
    if (k == 0)
      break;
    x *= x;
  }
  if (i < 0)
    f = 1.0f / f;
  return f;
}
