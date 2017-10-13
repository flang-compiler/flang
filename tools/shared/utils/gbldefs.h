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

/**
 * \file
 * \brief gbldefs.h - syminit/symutil utility definitions
 */

#ifndef INIT
#define TM_I8
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define CNULL ((char *)0)

#define MAXIDLEN 31

#define FIELD unsigned

typedef unsigned short ILM_T;

#if defined(_WIN32) || defined(HOST_WIN)
#define DCL_INT8(name) int name : 8
#define DCL_UINT8(name) FIELD name : 8
#define DCL_INT16(name) int name : 16
#define DCL_UINT16(name) unsigned name : 16
#else
#define DCL_INT8(name) char name
#define DCL_UINT8(name) FIELD name : 8
#define DCL_INT16(name) short int name
#define DCL_UINT16(name) unsigned short int name
#endif

/* define a host type which represents 'size_t' for array extents. */
#define ISZ_T BIGINT
#define UISZ_T BIGUINT
#define ISZ_PF BIGIPFSZ

typedef int LOGICAL;
#undef TRUE
#define TRUE 1
#undef FALSE
#define FALSE 0

#define BCOPY(p, q, dt, n) memcpy((char *)(p), (char *)(q), (sizeof(dt) * (n)))
#define BZERO(p, dt, n) memset((char *)(p), 0, (sizeof(dt) * (n)))
#define FREE(p) free((char *)p)

#define NEW(p, dt, n)                                       \
  if ((p = (dt *)malloc((UINT)(sizeof(dt) * (n)))) == NULL) \
    symini_interr("out of memory", 0, 4);                   \
  else

#define NEED(n, p, dt, size, newsize)                                       \
  if (n > size) {                                                           \
    if ((p = (dt *)realloc((char *)p, ((UINT)((newsize) * sizeof(dt))))) == \
        NULL)                                                               \
      symini_interr("out of memory", 0, 4);                                 \
    size = newsize;                                                         \
  } else
