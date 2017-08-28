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

CMPLXFUNC_C_C(__mth_i_cpowc)
{
  CMPLXARGS_C_C;
  float logr, logi, x, y, z, w;

  logr = LOGF(HYPOTF(real1, imag1));
  logi = ATAN2F(imag1, real1);

  x = EXPF(logr * real2 - logi * imag2);
  y = logr * imag2 + logi * real2;

  z = x * COSF(y);
  w = x * SINF(y);
  CRETURN_F_F(z, w);
}
