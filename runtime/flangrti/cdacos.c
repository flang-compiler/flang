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

/* inhibit floating point copy propagation */
#pragma global - Mx, 6, 0x100

#include "mthdecls.h"

ZMPLXFUNC_Z(__mth_i_cdacos)
{
  ZMPLXARGS_Z;
  #ifndef _WIN32
  complex double d = real + imag * I;
  #else
  _Dcomplex d = {real, imag};
  #endif
  d = cacos(d);
  ZRETURN_Z(d);
}
