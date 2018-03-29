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

ZMPLXFUNC_Z_K(__mth_i_cdpowk)
{
  ZMPLXARGS_Z_K;
  long long k;
  double fr, fi, gr, gi, tr, ti;
  double complex z;
  static const double complex c1plusi0 = 1.0 + I*0;

  fr = 1;
  fi = 0;
  k = i;
  gr = real;
  gi = imag;
  if (k < 0)
    k = -k;
  while (k) {
    if (k & 1) {
      tr = fr * gr - fi * gi;
      ti = fr * gi + fi * gr;
      fr = tr;
      fi = ti;
    }
    k >>= 1;
    tr = gr * gr - gi * gi;
    ti = 2.0 * gr * gi;
    gr = tr;
    gi = ti;
  }

  z = fr + I*fi;
  if (i < 0) {
    ZMPLX_CALL_ZR_Z_Z(__mth_i_cddiv,z,c1plusi0,z);
  }
  ZRETURN_Z(z);

}
