/* 
 * Copyright (c) 2003-2018, NVIDIA CORPORATION.  All rights reserved.
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


#include "directives.h"

#if defined(TARGET_WIN_X8664)
#define IN F1
#define SS I2
#define CC I3
#else
#define IN F1
#define SS I1
#define CC I2
#endif

	.text
/*
 *  <double, double> __mth_i_dsincos(double f)
 */

	ALN_FUNC
	.globl	ENT(__mth_i_dsincos)
ENT(__mth_i_dsincos):		/* %xmm0 contains f */
	subq	$24,%rsp
	leaq	(%rsp),SS	/* addr where to store double precision sin */
	leaq	8(%rsp),CC	/* addr where to store double precision cos */
	CALL	(ENT(__mth_dsincos))
	movsd	0(%rsp),%xmm0	/* sin */
	movsd	8(%rsp),%xmm1	/* cos */
	addq	$24,%rsp
	ret
	ELF_FUNC(__mth_i_dsincos)
	ELF_SIZE(__mth_i_dsincos)

/*
 *  <double, double> __mth_i_dsincos_avx2(double f)
 */

	ALN_FUNC
	.globl	ENT(__mth_i_dsincos_avx2)
ENT(__mth_i_dsincos_avx2):		/* %xmm0 contains f */
	subq	$24,%rsp
	leaq	(%rsp),SS	/* addr where to store double precision sin */
	leaq	8(%rsp),CC	/* addr where to store double precision cos */
#if	defined(TARGET_WIN_X8664)
#warning	__mth_dsincos_avx2 not available for TARGET_WIN_X8664
	ud2
#else
	CALL	(ENT(__mth_dsincos_avx2))
#endif
	movsd	0(%rsp),%xmm0	/* sin */
	movsd	8(%rsp),%xmm1	/* cos */
	addq	$24,%rsp
	ret
	ELF_FUNC(__mth_i_dsincos_avx2)
	ELF_SIZE(__mth_i_dsincos_avx2)
