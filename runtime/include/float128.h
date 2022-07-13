/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

#ifndef _FLOAT128_H_
#define _FLOAT128_H_

#if defined(TARGET_POWER) || defined(TARGET_X8664)
typedef __float128 float128_t;
#else
/* __float128 is not available on AArch64 or other generic targets;
   on AArch64 at least, long double is 128 bits in size. */
typedef long double float128_t;
#endif

#endif /* _FLOAT128_H_ */
