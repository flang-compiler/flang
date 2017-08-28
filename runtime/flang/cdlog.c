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

ZMPLXFUNC_Z(__mth_i_cdlog)
{
  ZMPLXARGS_Z;
  double x, y;
  /*
  call libm's atan2 may cause ieee_invalid & ieee_overflow to
  be set (f19305)
  x = atan2(imag, real);
  Call our version, which for x64, is in rte/pgc/hammer/src-amd/datan2.c
  */
  x = __mth_i_datan2(imag, real);
  y = log(hypot(real, imag));
  ZRETURN_D_D(y, x);
}
