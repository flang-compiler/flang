/*
 * Copyright (c) 2015-2018, NVIDIA CORPORATION.  All rights reserved.
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
 * \brief Definitions of all (c/c++/fortran) language features
 * that could depend on target, abi, etc.
 */

/*
 *
 * Examples:
 * + target is 64-bit
 * + for 64-bit x86 targets, the  C data type long is 64-bit except for WIN.
 * + pointer-size integer type
 * ...
 * Typically, these features could be appear in the source code using
 * conditional compilation based on certain combinations of TARGET_ checks.
 * The idea is an appropriately named feature would be defined in this
 * file and uses those same TARGET_checks.
 *
 * CURRENT STATUS - this hasn't been deployed yet; need to decide where this
 * file is included, what includes it might need, ...
 *
 * NOTES:
 *   TARGET_X8632 => 32-bit x86
 *   X86_32       => new 32-bit x86 CG
 *   I386         => old 32-bit x86 CG
 */

/*
 *  Determine CPU target size
 */
#define PG_LM_PLATFORM
#define TARGET_64BIT 1

/*
 * 64-bit CPU targets
 */
#define TARGET_PTRSIZE 8

#if !defined(TARGET_WIN)
/***** platform.h/gbldefs.h defines LONG_IS_64 to be 0 or 1 *****/
#define TARGET_LONGSIZE 8
#else
#define TARGET_LONGSIZE 4
#endif

/* Uniform structure assigments/moves ILI */
#define USE_GSMOVE XBIT(2,0x800000)

/* ETLS and threadprivate related features */
/* By default, prevent ETLS/TLS threadprivate usage */
#define XBIT_TLS_THREADPRIVATE 0
#define XBIT_ETLS 0
#define XBIT_TLS_AUTOPRIV 0

/*
 * Non-unifrom FP transformations
 */
#define XBIT_NOUNIFORM XBIT(15,0x200)

/*
 * Enable nodepchk similar to loop pragma/directive for OpenMP simd
 */
#define TARGET_KMPC
#define ALLOW_NODEPCHK_SIMD (!XBIT(69,0x100000))

/*
 * Strict adherence to OpenACC standard.
 */
#define ACCDEPRECATE XBIT(186, 0x80)
#define ACCSTRICT XBIT(186,0x100000)
#define ACCVERYSTRICT XBIT(186,0x200000)
