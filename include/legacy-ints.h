/*
 * Copyright (c) 2016-2018, NVIDIA CORPORATION.  All rights reserved.
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
 * \brief Legacy integer type
 *
 *  Some legacy types from scutil.h.
 */

#ifndef LEGACY_INTS_H_
#define LEGACY_INTS_H_
#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

typedef int INT;	/* native integer at least 32 bits */
typedef unsigned UINT;	/* unsigned 32 bit native integer */
typedef void VOID;

/* BIGINT is a mess.  It's defined as INT on many 32-bit targets,
 * despite comments that read "native integer at least 64 bits"
 * (as if things would still work if 128-bit integers were used).
 * Defining it as int64_t seems like a good idea but it leads to warnings
 * in nonportable code that really expects long.
 */
typedef long BIGINT;		/* native integer "at least 64 bits" */
typedef unsigned long BIGUINT;	/* native unsigned integer "at least 64 bits" */

typedef int64_t BIGINT64;	/* 64-bit native integer */
typedef uint64_t BIGUINT64;	/* 64-bit native unsigned integer */
typedef uint64_t BITMASK64;	/* native 64-bit unsigned int */

typedef int32_t DBLINT64[2];	/* signed 64-bit 2's complement integer: [0]
				 * - most significant 32 bits, including sign
				 * [1] - least significant 32 bits */
typedef uint32_t DBLUINT64[2];	/* unsigned 64-bit integer: [0] - most
				 * significant 32 bits [1] - least
				 * significant 32 bits */

void bgitoi64(int64_t x, DBLINT64 res);
int64_t i64tobgi(DBLINT64 x);

#define BIGIPFSZ "l" /* used to define ISZ_PF */

#ifdef __cplusplus
}
#endif
#endif /* LEGACY_INTS_H_ */
