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

int
__mth_i_ipoppari(int ii, int size)
{
  switch (size) {
  default:
    ii ^= ii >> 16;
    ii ^= ii >> 8;
    break;
  case 2:
    ii ^= ii >> 8;
    break;
  case 1:
    break;
  }
  ii ^= ii >> 4;
  ii ^= ii >> 2;
  ii ^= ii >> 1;
  return ii & 1;
}
