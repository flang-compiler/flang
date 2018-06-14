/*
 * Copyright (c) 2014-2017, NVIDIA CORPORATION.  All rights reserved.
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
__mth_i_knint(float d)
{
  if (d > 0)
    return ((d < 8388608.f) ? (_LONGLONG_T)(d + 0.5f) : (_LONGLONG_T)(d));
  else
    return ((d > -8388608.f) ? (_LONGLONG_T)(d - 0.5f) : (_LONGLONG_T)(d));
}
