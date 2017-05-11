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

#include "stdioInterf.h"
#include "fioMacros.h"

/* local gather functions */

static void
local_gather_INT1(int n, __INT1_T *dst, __INT1_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

static void
local_gather_INT2(int n, __INT2_T *dst, __INT2_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

static void
local_gather_INT4(int n, __INT4_T *dst, __INT4_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

static void
local_gather_INT8(int n, __INT8_T *dst, __INT8_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

static void
local_gather_LOG1(int n, __LOG1_T *dst, __LOG1_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

static void
local_gather_LOG2(int n, __LOG2_T *dst, __LOG2_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

static void
local_gather_LOG4(int n, __LOG4_T *dst, __LOG4_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

static void
local_gather_LOG8(int n, __LOG8_T *dst, __LOG8_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

static void
local_gather_REAL4(int n, __REAL4_T *dst, __REAL4_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

static void
local_gather_REAL8(int n, __REAL8_T *dst, __REAL8_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

static void
local_gather_REAL16(int n, __REAL16_T *dst, __REAL16_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

static void
local_gather_CPLX8(int n, __CPLX8_T *dst, __CPLX8_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

static void
local_gather_CPLX16(int n, __CPLX16_T *dst, __CPLX16_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

static void
local_gather_CPLX32(int n, __CPLX32_T *dst, __CPLX32_T *src, int *gv)
{
  int i;
  for (i = 0; i < n; ++i)
    dst[i] = src[gv[i]];
}

void (*__fort_local_gather[__NTYPES])() = {
    NULL,                /*     no type (absent optional argument) */
    NULL,                /* C   signed short */
    NULL,                /* C   unsigned short */
    NULL,                /* C   signed int */
    NULL,                /* C   unsigned int */
    NULL,                /* C   signed long int */
    NULL,                /* C   unsigned long int */
    NULL,                /* C   float */
    NULL,                /* C   double */
    local_gather_CPLX8,  /*   F complex*8 (2x real*4) */
    local_gather_CPLX16, /*   F complex*16 (2x real*8) */
    NULL,                /* C   signed char */
    NULL,                /* C   unsigned char */
    NULL,                /* C   long double */
    NULL,                /*   F character */
    NULL,                /* C   long long */
    NULL,                /* C   unsigned long long */
    local_gather_LOG1,   /*   F logical*1 */
    local_gather_LOG2,   /*   F logical*2 */
    local_gather_LOG4,   /*   F logical*4 */
    local_gather_LOG8,   /*   F logical*8 */
    NULL,                /*   F typeless */
    NULL,                /*   F double typeless */
    NULL,                /*   F ncharacter - kanji */
    local_gather_INT2,   /*   F integer*2 */
    local_gather_INT4,   /*   F integer*4, integer */
    local_gather_INT8,   /*   F integer*8 */
    local_gather_REAL4,  /*   F real*4, real */
    local_gather_REAL8,  /*   F real*8, double precision */
    local_gather_REAL16, /*   F real*16 */
    local_gather_CPLX32, /*   F complex*32 (2x real*16) */
    NULL,                /*   F quad typeless */
    local_gather_INT1,   /*   F integer*1 */
    NULL                 /*   F derived type */
};
