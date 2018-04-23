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

#include "mthdecls.h"

/* For X86-64 architectures, cdexp is defined in fastmath.s */

#if (! defined (TARGET_X8664) && ! defined(LINUX8664))
ZMPLXFUNC_Z(__mth_i_cdexp)
{
  ZMPLXARGS_Z;
  double x, y, z;
  x = exp(real);
  __mth_dsincos(imag, &z, &y);
  y *= x;
  z *= x;
  ZRETURN_D_D(y, z); /* should leave y & z in appropriate
                  * registers */
}
#endif
