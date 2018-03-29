/* 
 * Copyright (c) 2002-2018, NVIDIA CORPORATION.  All rights reserved.
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


#if 0
typedef union {
    float        f;
    unsigned int i;
} FPI;

#define EXPBIAS  127
#define MANTBITS 23
#define GET_EXP(u) (int)(((u) & 0x7fffffff) >> MANTBITS)

float
__mth_i_aint( float xx )
{
    unsigned int  ux, mask;
    int           xexp;
    float         x;
    FPI          fpi;

    x = xx;
    fpi.f = x;
    ux = fpi.i;

    xexp = GET_EXP(ux) - EXPBIAS;
    if ( xexp < 0 ) {
	/* |x| < 0 => zero with the original sign */
	fpi.i = ux & 0x80000000;
    }
    else if ( xexp < MANTBITS ) {
	/* 1 <= |x| < 2^24:
	 *    just mask out the trailing bits of the mantissa beyond the
	 *    range of the exponent; mask out the exponent field as well.
	 */
	mask = (1 << ( MANTBITS - xexp )) - 1;
	fpi.i = ux & ~mask;
    }
    /* else for illegal input, nan, inf, overflow, ...; just return it */

    return fpi.f;
}

#endif

#include "directives.h"

	.text
	ALN_FUNC
	.globl ENT(__mth_i_aint)
ENT(__mth_i_aint):
	RZ_PUSH
	movss	%xmm0, RZ_OFF(4)(%rsp)
	movl	RZ_OFF(4)(%rsp), %ecx
	movl	%ecx, %eax
	movl	%ecx, %edx
	andl	$2147483647, %eax
	shrl	$23, %eax
	subl	$127, %eax
	js	.L5
	cmpl	$22, %eax
	jg	.L3
	movl	$23, %ecx
	subl	%eax, %ecx
	movl	$-1, %eax
	sall	%cl, %eax
	andl	%eax, %edx
.L3:
	movl	%edx, RZ_OFF(4)(%rsp)
	movss	RZ_OFF(4)(%rsp), %xmm0
	RZ_POP
	ret
	ALN_FUNC
.L5:
	andl	$-2147483648, %edx
	movl	%edx, RZ_OFF(4)(%rsp)
	movss	RZ_OFF(4)(%rsp), %xmm0
	RZ_POP
	ret
	ELF_FUNC(__mth_i_aint)
	ELF_SIZE(__mth_i_aint)
