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

CMPLXFUNC_C(__mth_i_ccos)
{
  CMPLXARGS_C;
  float x, y;
  /*
  x = COSF(real) * COSHF(imag);
  y = -SINF(real) * SINHF(imag);
  */
  x = COSF(real);
  y = SINF(real);
  x = x * COSHF(imag);
  y = -y * SINHF(imag);
  CRETURN_F_F(x, y);
}
