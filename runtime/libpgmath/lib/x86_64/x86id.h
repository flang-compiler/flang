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
#define X86IDFN(n) __Cpuid_ ## n
#else
#define X86IDFN(n) n
#endif

extern int X86IDFN(is_intel)(void);	/* return 0 or 1 */
extern int X86IDFN(is_amd)(void);	/* return 0 or 1 */
extern int X86IDFN(is_ip6)(void);	/* return 0 or 1 */
extern int X86IDFN(is_sse)(void);	/* return 0 or 1 */
extern int X86IDFN(is_sse2)(void);	/* return 0 or 1 */
extern int X86IDFN(is_sse3)(void);	/* return 0 or 1 */
extern int X86IDFN(is_ssse3)(void);	/* return 0 or 1 */
extern int X86IDFN(is_sse4a)(void);	/* return 0 or 1 */
extern int X86IDFN(is_sse41)(void);	/* return 0 or 1 */
extern int X86IDFN(is_sse42)(void);	/* return 0 or 1 */
extern int X86IDFN(is_aes)(void);	/* return 0 or 1 */
extern int X86IDFN(is_avx)(void);	/* return 0 or 1 */
extern int X86IDFN(is_avx2)(void);	/* return 0 or 1 */
extern int X86IDFN(is_avx512f)(void);	/* return 0 or 1 */
extern int X86IDFN(is_avx512vl)(void);	/* return 0 or 1 */
extern int X86IDFN(is_fma)(void);	/* return 0 or 1 */
extern int X86IDFN(is_fma4)(void);	/* return 0 or 1 */
extern int X86IDFN(is_ht)(void);	/* return 0 .. logical processor count */
extern int X86IDFN(is_athlon)(void);	/* return 0 or 1 */
extern int X86IDFN(is_hammer)(void);	/* return 0 or 1 */
extern int X86IDFN(is_gh)(void);	/* return 0 or 1 */
extern int X86IDFN(is_gh_a)(void);	/* return 0 or 1 */
extern int X86IDFN(is_gh_b)(void);	/* return 0 or 1 */
extern int X86IDFN(is_shanghai)(void);	/* return 0 or 1 */
extern int X86IDFN(is_istanbul)(void);	/* return 0 or 1 */
extern int X86IDFN(is_bulldozer)(void);	/* return 0 or 1 */
extern int X86IDFN(is_piledriver)(void);/* return 0 or 1 */
extern int X86IDFN(is_k7)(void);	/* return 0 or 1 */
extern int X86IDFN(is_ia32e)(void);	/* return 0 or 1 */
extern int X86IDFN(is_p4)(void);	/* return 0 or 1 */
extern int X86IDFN(is_knl)(void);	/* return 0 or 1 */
extern int X86IDFN(is_x86_64)(void);	/* return 0 or 1 */
extern int X86IDFN(get_cachesize)(void);
extern char *X86IDFN(get_processor_name)(void);

#endif /* X86ID_H_ */
