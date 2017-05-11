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

/* scalar_copy.c - scalar element copy routines */

#include "stdioInterf.h"
#include "fioMacros.h"

static void
copy_none(__SHORT_T *rp, __SHORT_T *sp, int size)
{
  __fort_abort("scalar_copy: undefined type");
}
static void
copy_short(__SHORT_T *rp, __SHORT_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_ushort(__USHORT_T *rp, __USHORT_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_cint(__CINT_T *rp, __CINT_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_uint(__UINT_T *rp, __UINT_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_long(__LONG_T *rp, __LONG_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_ulong(__ULONG_T *rp, __ULONG_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_float(__FLOAT_T *rp, __FLOAT_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_double(__DOUBLE_T *rp, __DOUBLE_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_cplx8(__CPLX8_T *rp, __CPLX8_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_cplx16(__CPLX16_T *rp, __CPLX16_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_char(__CHAR_T *rp, __CHAR_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_uchar(__UCHAR_T *rp, __UCHAR_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_longdouble(__LONGDOUBLE_T *rp, __LONGDOUBLE_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_longlong(__LONGLONG_T *rp, __LONGLONG_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_ulonglong(__ULONGLONG_T *rp, __ULONGLONG_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_log1(__LOG1_T *rp, __LOG1_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_log2(__LOG2_T *rp, __LOG2_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_log4(__LOG4_T *rp, __LOG4_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_log8(__LOG8_T *rp, __LOG8_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_word4(__WORD4_T *rp, __WORD4_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_word8(__WORD8_T *rp, __WORD8_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_nchar(__NCHAR_T *rp, __NCHAR_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_int2(__INT2_T *rp, __INT2_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_int4(__INT4_T *rp, __INT4_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_int8(__INT8_T *rp, __INT8_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_real4(__REAL4_T *rp, __REAL4_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_real8(__REAL8_T *rp, __REAL8_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_real16(__REAL16_T *rp, __REAL16_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_cplx32(__CPLX32_T *rp, __CPLX32_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_word16(__WORD16_T *rp, __WORD16_T *sp, int size)
{
  *rp = *sp;
}
static void
copy_int1(__INT1_T *rp, __INT1_T *sp, int size)
{
  *rp = *sp;
}

static void copy_bytes(char *, char *, int);

void (*__fort_scalar_copy[__NTYPES])() = {
    copy_none,       /*     no type (absent optional argument) */
    copy_short,      /* C   signed short */
    copy_ushort,     /* C   unsigned short */
    copy_cint,       /* C   signed int */
    copy_uint,       /* C   unsigned int */
    copy_long,       /* C   signed long int */
    copy_ulong,      /* C   unsigned long int */
    copy_float,      /* C   float */
    copy_double,     /* C   double */
    copy_cplx8,      /*   F complex*8 (2x real*4) */
    copy_cplx16,     /*   F complex*16 (2x real*8) */
    copy_char,       /* C   signed char */
    copy_uchar,      /* C   unsigned char */
    copy_longdouble, /* C   long double */
    copy_bytes,      /*   F character */
    copy_longlong,   /* C   long long */
    copy_ulonglong,  /* C   unsigned long long */
    copy_log1,       /*   F logical*1 */
    copy_log2,       /*   F logical*2 */
    copy_log4,       /*   F logical*4 */
    copy_log8,       /*   F logical*8 */
    copy_word4,      /*   F typeless */
    copy_word8,      /*   F double typeless */
    copy_nchar,      /*   F ncharacter - kanji */
    copy_int2,       /*   F integer*2 */
    copy_int4,       /*   F integer*4, integer */
    copy_int8,       /*   F integer*8 */
    copy_real4,      /*   F real*4, real */
    copy_real8,      /*   F real*8, double precision */
    copy_real16,     /*   F real*16 */
    copy_cplx32,     /*   F complex*32 (2x real*16) */
    copy_word16,     /*   F quad typeless */
    copy_int1,       /*   F integer*1 */
    copy_bytes       /*   F derived type */
};

static void
copy_bytes(char *to, char *fr, int n)
{
  memmove(to, fr, n);
}
