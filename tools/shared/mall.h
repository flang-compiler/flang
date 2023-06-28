/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

#ifndef MALL_H_
#define MALL_H_

#ifdef __cplusplus
extern "C"
{
#endif

#include "gbldefs.h"

/**
   \brief ...
 */
char *sccalloc(BIGUINT64 nbytes);

/**
   \brief ...
 */
char *sccrelal(char *pp, BIGUINT64 nbytes);

#if DEBUG
/**
   \brief ...
 */
void bjunk(void *p, BIGUINT64 n);
#endif

/**
   \brief ...
 */
void sccfree(char *ap);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // MALL_H_
