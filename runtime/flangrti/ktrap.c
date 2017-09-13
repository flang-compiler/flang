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

/** \file
 *  \brief IEEE trap support
 */

#ifndef TARGET_WIN

#include <fenv.h>

#if !defined(TARGET_OSX)
/* Use the LINUX interpretation of 'weak references' */
extern int __ktrapval __attribute__ ((weak));

#else
/* Use the OSX feature of 'weak definitions' */
       int __ktrapval __attribute__ ((weak));
#endif

void
__ktrap(void)
{
  if (&__ktrapval != 0) {
    int bv = __ktrapval;
    if (bv != 0) {
      /*
       *  -Ktrap      bv
       *    fp      0x001  => inv | divz | ovf
       *    inv     0x008
       *    divz    0x020
       *    ovf     0x040
       *    unf     0x080
       *    inexact 0x100
       */
      int excepts = 0;
      if (bv & 0x001)
        bv = 0x008 | 0x020 | 0x040;
      if (bv & 0x008)
        excepts |= FE_INVALID;
      if (bv & 0x020)
        excepts |= FE_DIVBYZERO;
      if (bv & 0x040)
        excepts |= FE_OVERFLOW;
      if (bv & 0x080)
        excepts |= FE_UNDERFLOW;
      if (bv & 0x100)
        excepts |= FE_INEXACT;
      feenableexcept(excepts);  /* glibc 2.2 extension to fenv.h */
    }
  }
}

#else
void
__ktrap(void)
{
}
#endif
