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

CMPLXFUNC_C_C(__mth_i_cdiv)
{
  CMPLXARGS_C_C;
  float x, y;
  float r, d, r_mag, i_mag;

  r_mag = real2;
  if (r_mag < 0)
    r_mag = -r_mag;
  i_mag = imag2;
  if (i_mag < 0)
    i_mag = -i_mag;
  /* avoid overflow */
  if (r_mag <= i_mag) {
    r = real2 / imag2;
    d = 1.0f / (imag2 * (1 + r * r));
    x = (real1 * r + imag1) * d;
    y = (imag1 * r - real1) * d;
  } else {
    r = imag2 / real2;
    d = 1.0f / (real2 * (1 + r * r));
    x = (real1 + imag1 * r) * d;
    y = (imag1 - real1 * r) * d;
  }
  CRETURN_F_F(x, y);
}
