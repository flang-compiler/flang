/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/**
 * \file
 * \brief  float128.h - Define float128_t on all platforms.
 */

#ifndef _FLOAT128_H_
#define _FLOAT128_H_

#include <stdint.h>

/* See https://gcc.gnu.org/onlinedocs/gcc/Floating-Types.html. */
#if defined(LINUX8664) || (defined(TARGET_X8664) && !defined(TARGET_WIN))
typedef __float128 float128_t;
typedef _Complex float __attribute__((mode(TC))) quad_complex_t;
#elif defined(TARGET_LINUX_POWER)
typedef __float128 float128_t;
typedef _Complex float __attribute__((mode(KC))) quad_complex_t;
#else
/* __float128 is not available on AArch64 or other generic targets;
   on AArch64 at least, long double is 128 bits in size. */
typedef long double float128_t;
typedef long double _Complex quad_complex_t;
#endif
typedef uint64_t ui64arr2_t[2];

#endif /* _FLOAT128_H_ */
