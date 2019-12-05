/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/** \file
 * \brief Declarations common across all programs for all hosts and targets.
 */

#ifndef UNIVERSAL_DEFS_H_
#define UNIVERSAL_DEFS_H_

#ifdef __cplusplus

#ifdef SHADOW_BUILD
#define BEGIN_DECL_WITH_C_LINKAGE
#define END_DECL_WITH_C_LINKAGE
#else
#define BEGIN_DECL_WITH_C_LINKAGE extern "C" {
#define END_DECL_WITH_C_LINKAGE }
#endif

#ifndef INLINE
#define INLINE inline
#define HAVE_INLINE 1
#endif

#else // !__cplusplus

#define BEGIN_DECL_WITH_C_LINKAGE
#define END_DECL_WITH_C_LINKAGE

/* Linux and MacOS environments provide <stdbool.h> even for C89.
   Microsoft OpenTools 10 does not, even for C99. */
#if __linux__ || __APPLE__ || __STDC_VERSION__ >= 199901L && !__PGI_TOOLS10
#include <stdbool.h>
#else
typedef char bool;
#define true 1
#define false 0
#endif

#ifndef INLINE
#if defined(__GNUC__) || (__PGIC__ + 0 > 14)
#define INLINE __inline__
#define HAVE_INLINE 1
#else
#define INLINE
#endif
#endif

#endif /* __cplusplus */

#endif /* UNIVERSAL_DEFS_H_ */
