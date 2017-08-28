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

CMPLXFUNC_C(__mth_i_cexp)
{
  CMPLXARGS_C;
  float x, y, z;
  x = EXPF(real);
  __mth_sincos(imag, &z, &y);
  y *= x;
  z *= x;
  CRETURN_F_F(y, z); /* should leave y & z in appropriate
                  * registers */
}
