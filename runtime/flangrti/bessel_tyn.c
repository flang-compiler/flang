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

/*  bessel_tyn.c - implement float F2008 bessel_yn transformational intrinsic */
#include "mthdecls.h"
#include "stdioInterf.h"

void
f90_bessel_yn(float *rslts, int *n1, int *n2, float *x)
{
  int i;
  float *rslt_p;

  for (i = *n1, rslt_p = rslts; i <= *n2; i++, rslt_p++) {
    switch (i) {
    case 0:
      *rslt_p = BESSEL_Y0F(*x);
      break;
    case 1:
      *rslt_p = BESSEL_Y1F(*x);
      break;
    default:
      *rslt_p = BESSEL_YNF(i, *x);
      break;
    }
  }
}

