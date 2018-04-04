/*
 * Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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

#error  Single precision - generic sincos() will not work on X86-64 systems.

#include "mthdecls.h"

/*
 * Generic implementation of intrinsic sincos.
 *
 * Compiler expects two return values, thus using the complex type to implement
 * the return sequence.
 */

float complex __mth_i_sincos(float a)
{
  float s, c;
  __mth_sincos(a, &s, &c);
  return s + I * c;
}
