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

/* clang-format off */

/**
 * \file
 * \brief ent3f.h macros for building RTE routine names and arg lists
 */

#undef DCHAR
#undef DCLEN
#undef CADR
#undef CLEN

/* macros for entries */
#if defined(WINNT) && !defined(WIN64) && !defined(UXOBJS)

#pragma global - x 121 0x20000
#define ENT3F(UC, LC) __attribute__((stdcall)) UC
#define ENT3FSU(UC, LC) __attribute__((stdcall)) UC##_
/* macros to declare character arguments */
#define DCHAR(ARG) char *ARG##_adr, int ARG##_len
#define DCLEN(ARG)

#else

#define ENT3F(UC, LC) LC##_
#define ENT3FSU(UC, LC) LC##__
/* macros to declare character arguments */
#define DCHAR(ARG) char *ARG##_adr
#define DCLEN(ARG) , int ARG##_len

#endif

#if defined(WIN32) || defined(WIN64)
#define j0 _j0
#define j1 _j1
#define jn _jn
#define y0 _y0
#define y1 _y1
#define yn _yn
#define access _access
#define chdir _chdir
#define chmod _chmod
#define getpid _getpid
#define putenv _putenv
#define unlink _unlink
#endif

/* macros to access character arguments */
#define CADR(ARG) (ARG##_adr)
#define CLEN(ARG) (ARG##_len)

/* declarations in runtime must match declarations in MS msvcrt.dll
 * to achieve consistent DLL linkage.
 */
#define WIN_CDECL
#define WIN_MSVCRT_IMP extern
