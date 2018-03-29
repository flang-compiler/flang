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
#if defined(WIN64)
typedef long long I64;
typedef unsigned long long UI64; 
#else
typedef long I64;
typedef unsigned long UI64; 
#endif

typedef union {
    double f;
    UI64   i;
} FPI;

#define EXPBIAS 1023
#define MANTBITS 52
#define GET_EXP(u) (I64)(((u) & 0x7ff0000000000000) >> MANTBITS)

double
__mth_i_dint (double xx)
{
    I64          xexp;
    UI64         ux, mask;
    double       x;
    FPI         fpi;

    x = xx;
    fpi.f = x;
    ux = fpi.i;
    xexp = GET_EXP(ux) - EXPBIAS;
    if (xexp < 0 ) {
	/* |x| < 0  =>  zero with the original sign */
	fpi.i = ( ux & 0x8000000000000000 );
    }
    else if (xexp < MANTBITS) {
	/* 1 <= |x| < 2^53:
	 *    just mask out the trailing bits of the mantiassa beyond the
	 *    range of the exponent; mask out the exponent field as well.
	 */
	mask = ((UI64)1 << ( MANTBITS - xexp )) - 1;
	fpi.i = ux & ~mask;
    }
    /* else illegal input, nan, inf, overflow, ...; just return it */ 

    return fpi.f;
}
#endif

#include "directives.h"

	.text
	ALN_FUNC
	.globl	ENT(__mth_i_dint)
ENT(__mth_i_dint):
	RZ_PUSH
	movsd	%xmm0, RZ_OFF(8)(%rsp)
	movq	RZ_OFF(8)(%rsp), %rcx
	leaq	(%rcx,%rcx), %rax
	movq	%rcx, %rdx
	shrq	$53, %rax
	subq	$1023, %rax
	js	.L5
	cmpq	$51, %rax
	jg	.L3
	movl	$52, %ecx
	subl	%eax, %ecx
	movq	$-1, %rax
	salq	%cl, %rax
.L6:
	andq	%rax, %rdx
.L3:
	movq	%rdx, RZ_OFF(8)(%rsp)
	movlpd	RZ_OFF(8)(%rsp), %xmm2
	movsd	%xmm2, %xmm0
	RZ_POP
	ret
	ALN_FUNC
.L5:
	movabsq	$-9223372036854775808, %rax
	jmp	.L6
	ELF_FUNC(__mth_i_dint)
	ELF_SIZE(__mth_i_dint)
