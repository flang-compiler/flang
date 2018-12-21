/* 
 * Copyright (c) 2007-2018, NVIDIA CORPORATION.  All rights reserved.
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

#ifndef X86ID_H_
#define X86ID_H_

#if defined(FOR_LIBPGC)
#define X86IDFN_(l,r) l##r
#define X86IDFN(n) X86IDFN_(__Cpuid_,n)
#else
#define X86IDFN(n) n
#endif

#define	X86ID_IS_CACHED_UNDEF   0xffffffff

#if     ! defined(__ASSEMBLER__)

#define IS_CONCAT3_(l,m,r)  l##m##r
#define IS_CONCAT3(l,m,r)   IS_CONCAT3_(l,m,r)

#define IS_X86ID(f)                                                           \
    (X86IDFN(IS_CONCAT3(is_,f,_cached)) != X86ID_IS_CACHED_UNDEF) ?            \
        X86IDFN(IS_CONCAT3(is_,f,_cached)) :X86IDFN(IS_CONCAT3(is_,f,))()

/*
 * All the "_cached" varaibles are one of three values:
 * 1) IS_X86ID_CACHED_UNDEF:    not initialized
 * 2) false (0):                initialized and value is false
 * 3) true (1):                 initialized and value is true
 */

/*
 *  For Non-Windows based builds (Linux, OSX), the extern keyword
 *  gives the proper attribute for the global variables is_<FEATURE>_cached.
 *  But for Windows, we need to use MS' __declspec attribute.
 *  When building x86id.c which defines those global variables, we define the
 *  CPP object macro OBJ_WIN_X8664_IS_X86ID.
 */

#if     defined (TARGET_WIN_X8664) && defined(_DLL)
#   if      defined(OBJ_WIN_X8664_IS_X86ID)
#       define  DECLEXTERN  __declspec(dllexport)
#   else
#       define  DECLEXTERN  __declspec(dllimport)
#   endif
#else
#   define  DECLEXTERN  extern
#endif

DECLEXTERN	int X86IDFN(is_intel_cached);
DECLEXTERN	int X86IDFN(is_amd_cached);
DECLEXTERN	int X86IDFN(is_ip6_cached);
DECLEXTERN	int X86IDFN(is_sse_cached);
DECLEXTERN	int X86IDFN(is_sse2_cached);
DECLEXTERN	int X86IDFN(is_sse3_cached);
DECLEXTERN	int X86IDFN(is_ssse3_cached);
DECLEXTERN	int X86IDFN(is_sse4a_cached);
DECLEXTERN	int X86IDFN(is_sse41_cached);
DECLEXTERN	int X86IDFN(is_sse42_cached);
DECLEXTERN	int X86IDFN(is_aes_cached);
DECLEXTERN	int X86IDFN(is_avx_cached);
DECLEXTERN	int X86IDFN(is_avx2_cached);
DECLEXTERN	int X86IDFN(is_avx512_cached);
DECLEXTERN	int X86IDFN(is_avx512f_cached);
DECLEXTERN	int X86IDFN(is_avx512vl_cached);
DECLEXTERN	int X86IDFN(is_fma_cached);
DECLEXTERN	int X86IDFN(is_fma4_cached);
DECLEXTERN	int X86IDFN(is_ht_cached);
DECLEXTERN	int X86IDFN(is_athlon_cached);
DECLEXTERN	int X86IDFN(is_hammer_cached);
DECLEXTERN	int X86IDFN(is_gh_cached);
DECLEXTERN	int X86IDFN(is_gh_a_cached);
DECLEXTERN	int X86IDFN(is_gh_b_cached);
DECLEXTERN	int X86IDFN(is_shanghai_cached);
DECLEXTERN	int X86IDFN(is_istanbul_cached);
DECLEXTERN	int X86IDFN(is_bulldozer_cached);
DECLEXTERN	int X86IDFN(is_piledriver_cached);
DECLEXTERN	int X86IDFN(is_k7_cached);
DECLEXTERN	int X86IDFN(is_ia32e_cached);
DECLEXTERN	int X86IDFN(is_p4_cached);
DECLEXTERN	int X86IDFN(is_knl_cached);
DECLEXTERN	int X86IDFN(is_x86_64_cached);
DECLEXTERN	int X86IDFN(is_f16c_cached);

DECLEXTERN	int X86IDFN(is_intel)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_amd)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_ip6)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_sse)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_sse2)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_sse3)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_ssse3)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_sse4a)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_sse41)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_sse42)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_aes)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_avx)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_avx2)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_avx512)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_avx512f)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_avx512vl)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_fma)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_fma4)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_ht)(void);	/* return 0 .. logical processor count */
DECLEXTERN	int X86IDFN(is_athlon)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_hammer)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_gh)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_gh_a)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_gh_b)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_shanghai)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_istanbul)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_bulldozer)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_piledriver)(void);/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_k7)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_ia32e)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_p4)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_knl)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(is_x86_64)(void);	/* return 0 or 1 */
DECLEXTERN	int X86IDFN(get_cachesize)(void);
DECLEXTERN	int X86IDFN(is_f16c)(void);
DECLEXTERN	char *X86IDFN(get_processor_name)(void);

#if !defined(FOR_LIBPGC)
extern int get_cores(void);
#endif

#endif          /* ! defined(__ASSEMBLER__) */

#endif /* X86ID_H_ */
/* vim: set ts=4 expandtab: */
