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

/*
 *  <float, float> __mth_i_sincos_avx2(float f)
 *  <float, float> __mth_i_sincos(float f)
 *
 *	Entry:	(%xmm0) = f
 *
 *	Exit	(%xmm0) = sinf(f)
 *		(%xmm1) = cosf(f)
 */

	.text
	ALN_FUNC
	.globl	ENT(__mth_i_sincos_avx2)
ENT(__mth_i_sincos_avx2):		/* %xmm0 contains f */

#if	defined(TARGET_WIN_X8664)
#warning	__mth_sincos_avx2 not currently available for TARGET_WIN_X8664
	ud2
#else
	movq	1f(%rip),%rax	/* Address of mth_sincos_avx2 */
	jmp	2f		/* Jump to common __mth_i_sincos() logic */

#endif
	ELF_FUNC(__mth_i_sincos_avx2)
	ELF_SIZE(__mth_i_sincos_avx2)

#if	! defined(TARGET_WIN_X8664)
	.data
	ALN_QUAD
1:	.quad	ENT(__mth_sincos_avx2)
#endif

	.text
	.globl	ENT(__mth_i_sincos)
ENT(__mth_i_sincos):		/* %xmm0 contains f */

	movq	1f(%rip),%rax	/* Address of mth_sincos */

2:				/* If coming from __mth_sincos_avx2 */
	subq	$24,%rsp
	leaq	(%rsp),SS	/* addr where to store float precision sin */
	leaq	4(%rsp),CC	/* addr where to store float precision cos */
	call	*%rax		/* call __mth_i_sincos_avx2 or __mth_i_sincos */
	movss	0(%rsp),%xmm0	/* sin */
	movss	4(%rsp),%xmm1	/* cos */
	addq	$24,%rsp
	ret

	ELF_FUNC(__mth_i_sincos)
	ELF_SIZE(__mth_i_sincos)

	.data
	ALN_QUAD
1:	.quad	ENT(__mth_sincos)
