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

_LONGLONG_T
__mth_i_kleadz(_LONGLONG_T i)
{
  _ULONGLONG_T ui; /* unsigned representation of 'i' */
  int nz;          /* number of leading zero bits in 'i' */
  int k;

  ui = i;
  nz = 64;
  k = nz >> 1;
  while (k) {
    if (ui >> k) {
      ui >>= k;
      nz -= k;
    }
    k >>= 1;
  }
  if (ui)
    --nz;
  return nz;
}
