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


 
#include "directives.h"
 
/*
 *   float __mth_i_anint(float f)
 */
	.text
	ALN_FUNC
	.globl	ENT(__mth_i_anint)
ENT(__mth_i_anint):
	cvtss2sd	%xmm0, %xmm0
	movsd		.sgnb(%rip), %xmm1
	andpd		%xmm0, %xmm1
	movsd		.maxd(%rip), %xmm2
	movsd		.cnp5(%rip), %xmm4
	movsd		.mnp5(%rip), %xmm3
	xorpd		%xmm1, %xmm0
	movapd		%xmm0, %xmm5
	addsd		%xmm2, %xmm5
	subsd		%xmm2, %xmm5
	movapd		%xmm5, %xmm2
	subsd		%xmm0, %xmm2
	movapd		%xmm2, %xmm0
	cmplesd		%xmm3, %xmm2
	movapd		%xmm4, %xmm3
	addsd		%xmm3, %xmm3
	andpd		%xmm3, %xmm2
	cmpnlesd	%xmm4, %xmm0
	andpd		%xmm3, %xmm0
	subsd		%xmm0, %xmm5
	addsd		%xmm2, %xmm5
	orpd		%xmm1, %xmm5
	cvtsd2ss	%xmm5, %xmm0
	ret
	ALN_DBLE
.sgnb:
	.long 0x00000000,0x80000000
.maxd:
	.long 0x00000000,0x43300000
.cnp5:
	.long 0x00000000,0x3fe00000
.mnp5:
	.long 0x00000000,0xbfe00000
	ELF_FUNC(__mth_i_anint)
	ELF_SIZE(__mth_i_anint)
