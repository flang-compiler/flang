/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

#include "mthdecls.h"

#if !defined(_WIN64)
double
__mth_i_dmod(double f, double g)
{
/* Need to do this way until a bug in the Win64 fmod routine is fixed */
#if defined(_WIN64)
  return __fmth_i_dmod(f, g);
#else
  return fmod(f, g);
#endif
}
#endif
