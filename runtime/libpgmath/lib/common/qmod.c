/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/* Intrinsic function which take quad precision arguments. */

#include "mthdecls.h"

#ifndef WIN64
long double __mth_i_qmod(long double f, long double g)
{
/* Need to do this way until a bug in the Win64 fmod routine is fixed */
#if defined(WIN64)
  return __fmth_i_qmod(f, g);
#else
  return fmodl(f, g);
#endif
}
#endif
