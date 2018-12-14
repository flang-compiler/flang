/*
 * Copyright (c) 1997-2018, NVIDIA CORPORATION.  All rights reserved.
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

#include "dblint64.h"

/*
 * define a C type for long long so that the routines using this type
 * will always compile.  For those systems where long long isn't
 * supported, TM_I8 will not be defined, but at least the run-time routines
 * will compile.
 */

#define __HAVE_LONGLONG_T

#if defined(LINUX8664) || defined(OSX8664)
typedef long _LONGLONG_T;
typedef unsigned long _ULONGLONG_T;
#else
typedef long long _LONGLONG_T;
typedef unsigned long long _ULONGLONG_T;
#endif

#define I64_MSH(t) t[1]
#define I64_LSH(t) t[0]

int __ftn_32in64_;

#define VOID void

typedef union {
  DBLINT64 i;
  double d;
  _LONGLONG_T lv;
} INT64D;

#if defined(LINUX8664) || defined(OSX8664)
#define __I8RET_T long
#define UTL_I_I64RET(m, l)                                                     \
  {                                                                            \
    INT64D int64d;                                                             \
    I64_MSH(int64d.i) = m;                                                     \
    I64_LSH(int64d.i) = l;                                                     \
    return int64d.lv;                                                          \
  }
#elif defined(WIN64)
/* Someday, should only care if TM_I8 is defined */
#define __I8RET_T long long
#define UTL_I_I64RET(m, l)                                                     \
  {                                                                            \
    INT64D int64d;                                                             \
    I64_MSH(int64d.i) = m;                                                     \
    I64_LSH(int64d.i) = l;                                                     \
    return int64d.lv;                                                          \
  }
#else
#define __I8RET_T void
#define UTL_I_I64RET __utl_i_i64ret
extern VOID UTL_I_I64RET();
#endif
