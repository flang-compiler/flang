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
