/*
 * Copyright (c) 2016-2017, NVIDIA CORPORATION.  All rights reserved.
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

#include <math.h>

/* Assume little endian for now */
#define BITSDH(f) ((int *)&f)[1]
#define BITSDL(f) ((int *)&f)[0]
#define BSIGNF 0x80000000

double
__mth_i_dsign(double a, double b)
{
  double r;
  r = fabs(a);
  if (BITSDH(b) & BSIGNF) {
    /*r = -fabs(a);*/
    BITSDH(r) = BITSDH(r) | BSIGNF;
  }
  return r;
}
