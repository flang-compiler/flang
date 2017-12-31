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

/* These routines are included in linux and osx.
   I've implemented them so we can also claim support on Windows.
   Plus, we can standardize our support of F2003 ieee_exceptions
   and ieee_arithmetic modules across all platforms

   Plus, Cray is asking for them.  Not sure they know they are in Linux
   Hope to find out in mid-2009

   The OSX implementation and our implementation are similar.  Linux
   gnu does not use x87 in 64 bits, and doesn't seem to use mxcsr in
   32 bits.

     - Brent
*/
#include "stdioInterf.h"

double __mth_i_dround(double x);
float __mth_i_around(float x);
double __mth_i_dremainder(double x, double y);
float __mth_i_remainder(float x, float y);

double
__nearbyint(double x)
{
  return __mth_i_dround(x);
}

float
__nearbyintf(float x)
{
  return __mth_i_around(x);
}

double
__remainder(double x, double y)
{
  return __mth_i_dremainder(x, y);
}

float
__remainderf(float x, float y)
{
  return __mth_i_remainder(x, y);
}

#define _MAXDOUBLE (1.7976931348623157e+308)

double
__nextafter(double x, double y)
{
  double ex;
  unsigned int ix[2], ixh, ixl, iyh, iyl;
  int idflag, subflag;

  ex = y;
  iyl = *((int *)(&(ex)));
  iyh = *((int *)(&(ex)) + 1);

  ex = x;
  ixl = *((int *)(&(ex)));
  ixh = *((int *)(&(ex)) + 1);

  /* y is nan, return y */
  if (((iyh & 0x7ff00000) == 0x7ff00000) &&
      ((iyl != 0) || (iyh & 0x0fffff) != 0))
    return y;

  /* x is nan, return x */
  if (((ixh & 0x7ff00000) == 0x7ff00000) &&
      ((ixl != 0) || (ixh & 0x0fffff) != 0))
    return y;

  /* x infinity, return -_MAXDOUBLE, +_MAXDOUBLE, or x */
  if ((ixh & 0x7ff00000) == 0x7ff00000) {
    if ((iyh & 0x7f800000) == 0x7f800000) {
      if (!(ixh & 0x80000000)) {
        if (iyh & 0x80000000)
          /* inf, -inf */
          return _MAXDOUBLE;
      } else if (!(iyh & 0x80000000))
        /* -inf, +inf */
        return -_MAXDOUBLE;
      return x;
    }
    if (!(ixh & 0x80000000))
      /* +inf, any number */
      return _MAXDOUBLE;
    /* -inf, any number */
    return -_MAXDOUBLE;
  }

  if (x == y)
    return x;
  subflag = (x > y);

  if (((ixh & 0x7fffffff) == 0) && (ixl == 0)) {
    idflag = __fenv_fegetzerodenorm();
    if (idflag) {
      if (subflag) {
        ix[1] = 0x80100000;
        ix[0] = 0x00000000;
      } else {
        ix[1] = 0x00100000;
        ix[0] = 0x00000000;
      }
    } else {
      if (subflag) {
        ix[1] = 0x80000000;
        ix[0] = 0x00000001;
      } else {
        ix[1] = 0x00000000;
        ix[0] = 0x00000001;
      }
      ex = *((double *)ix);
      return x + ex; // underflow here
    }
  } else {
    if (ixh & 0x80000000)
      subflag = !subflag;
    if (((ixh & 0x7fffffff) == 0x00100000) && (ixl == 0) && (subflag)) {
      /* In this special case, setup a value to subtract
         so we cause exceptions to occur properly
      */
      idflag = __fenv_fegetzerodenorm();
      ix[1] = (ixh & 0x80000000);
      if (idflag) {
        ix[1] = ix[1] | 0x00800000;
        ix[0] = 0x00000000;
      } else {
        ix[0] = 0x00000001;
      }
      ex = *((double *)ix);
      return x - ex; // possible underflow here

    } else if (((ixh & 0x7fffffff) == 0x7fefffff) && (ixl == 0xffffffff) &&
               (!subflag)) {
      ix[1] = (ixh & 0xfca00000);
      ix[0] = 0;
      ex = *((double *)ix);
      return x + ex; // overflow to infinity
    } else {
      /* This is the normal case */
      if (subflag) {
        if (ixl == 0) {
          ix[0] = ixl - 1;
          ix[1] = ixh - 1;
        } else {
          ix[0] = ixl - 1;
          ix[1] = ixh;
        }
      } else {
        ix[0] = ixl + 1;
        if (ix[0] == 0) {
          ix[1] = ixh + 1;
        } else {
          ix[1] = ixh;
        }
      }
    }
  }
  ex = *((double *)ix);
  return ex;
}

#define _MAXFLOAT (3.40282347e+38F)

float
__nextafterf(float x, float y)
{
  float ex;
  unsigned int ix, iy, iz;
  int idflag, subflag;

  ex = y;
  iy = *((int *)(&(ex)));
  ex = x;
  ix = *((int *)(&(ex)));

  /* y is nan, return y */
  if (((iy & 0x7f800000) == 0x7f800000) && ((iy & 0x7fffff) != 0))
    return y;

  /* x is nan  return x */
  if (((ix & 0x7f800000) == 0x7f800000) && ((ix & 0x7fffff) != 0))
    return x;

  /* x infinity, return -_MAXFLOAT, +_MAXFLOAT, or x */
  if ((ix & 0x7f800000) == 0x7f800000) {
    if ((iy & 0x7f800000) == 0x7f800000) {
      if (!(ix & 0x80000000)) {
        if (iy & 0x80000000)
          /* inf, -inf */
          return _MAXFLOAT;
      } else if (!(iy & 0x80000000))
        /* -inf, +inf */
        return -_MAXFLOAT;
      return x;
    }
    if (!(ix & 0x80000000))
      /* +inf, any number */
      return _MAXFLOAT;
    /* -inf, any number */
    return -_MAXFLOAT;
  }

  if (x == y)
    return x;
  subflag = (x > y);

  if ((ix & 0x7fffffff) == 0) {
    idflag = __fenv_fegetzerodenorm();
    if (idflag) {
      if (subflag)
        iz = 0x80800000;
      else
        iz = 0x00800000;
    } else {
      if (subflag)
        iz = 0x80000001;
      else
        iz = 0x00000001;
      ex = *((float *)&iz);
      return x + ex; // underflow here
    }
  } else {
    if (ix & 0x80000000)
      subflag = !subflag;
    if (((ix & 0x7fffffff) == 0x00800000) && (subflag)) {
      idflag = __fenv_fegetzerodenorm();
      iz = (ix & 0x80000000);
      if (idflag)
        iz = iz | 0x00800000;
      else
        iz = iz | 0x00000001;
      ex = *((float *)&iz);
      return x - ex; // possible underflow here
    } else if (((ix & 0x7fffffff) == 0x7f7fffff) && (!subflag)) {
      iz = (ix & 0xf3000000);
      ex = *((float *)&iz);
      return x + ex; // overflow to infinity
    } else {
      if (subflag)
        iz = ix - 1;
      else
        iz = ix + 1;
    }
  }
  ex = *((float *)&iz);
  return ex;
}

double
__scalbn(double x, int i)
{
  /* Do the scaling in three parts.  Should allow for full range of
     scaling, but still will generate underflow/overflow where appropriate
  */
  double ex, fx;
  int ix[2], iy[2], iz[2];
  ix[1] = i;
  ix[0] = 0;
  iy[1] = 0;
  iy[0] = 0;
  iz[1] = 0;
  iz[0] = 0;
  if (i > 1000) {
    ix[1] = 1000;
    iy[1] = ((i - ix[1]) < 1000) ? i - ix[1] : 1000;
    iz[1] = ((i - ix[1] - iy[1]) < 1000) ? i - ix[1] - iy[1] : 1000;
  } else if (i < -1000) {
    ix[1] = -1000;
    iy[1] = ((i - ix[1]) > -1000) ? i - ix[1] : -1000;
    iz[1] = ((i - ix[1] - iy[1]) > -1000) ? i - ix[1] - iy[1] : -1000;
  }
  ix[1] = ix[1] + 1023;
  ix[1] = (ix[1] << 20);
  ex = *((double *)ix);
  fx = x * ex;
  if (iy[1] != 0) {
    iy[1] = iy[1] + 1023;
    iy[1] = (iy[1] << 20);
    ex = *((double *)iy);
    fx = fx * ex;
  }
  if (iz[1] != 0) {
    iz[1] = iz[1] + 1023;
    iz[1] = (iz[1] << 20);
    ex = *((double *)iz);
    fx = fx * ex;
  }
  return fx;
}

float
__scalbnf(float x, int i)
{
  /* Do the scaling in three parts.  Should allow for full range of
     scaling, but still will generate underflow/overflow where appropriate
  */
  float ex, fx;
  int ix, iy, iz;
  ix = i;
  iy = 0;
  iz = 0;
  if (i > 120) {
    ix = 120;
    iy = ((i - ix) < 120) ? i - ix : 120;
    iz = ((i - ix - iy) < 120) ? i - ix - iy : 120;
  } else if (i < -120) {
    ix = -120;
    iy = ((i - ix) > -120) ? i - ix : -120;
    iz = ((i - ix - iy) > -120) ? i - ix - iy : -120;
  }
  ix = ix + 127;
  ix = (ix << 23);
  ex = *((float *)&ix);
  fx = x * ex;
  if (iy != 0) {
    iy = iy + 127;
    iy = (iy << 23);
    ex = *((float *)&iy);
    fx = fx * ex;
  }
  if (iz != 0) {
    iz = iz + 127;
    iz = (iz << 23);
    ex = *((float *)&iz);
    fx = fx * ex;
  }
  return fx;
}

#ifdef WINNT
double
nearbyint(double x)
{
  return __nearbyint(x);
}
float
nearbyintf(float x)
{
  return __nearbyintf(x);
}
double
remainder(double x, double y)
{
  return __remainder(x, y);
}
float
remainderf(float x, float y)
{
  return __remainderf(x, y);
}
double
nextafter(double x, double y)
{
  return __nextafter(x, y);
}
float
nextafterf(float x, float y)
{
  return __nextafterf(x, y);
}
double
scalbn(double x, int i)
{
  return __scalbn(x, i);
}
float
scalbnf(float x, int i)
{
  return __scalbnf(x, i);
}
#endif
