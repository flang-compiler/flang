/* 
 * Copyright (c) 2009-2018, NVIDIA CORPORATION.  All rights reserved.
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

	.text
	ALN_FUNC
	.globl ENT(__mth_i_dround)
ENT(__mth_i_dround):
	RZ_PUSH
	movsd	%xmm0, RZ_OFF(8)(%rsp)
	movq	RZ_OFF(8)(%rsp), %rax
	shrq	$52, %rax
	andq	$2047, %rax
	cmpq	$1074, %rax
	jg	.L3
	cvtsd2si	%xmm0, %rax
	cvtsi2sd	%rax, %xmm0
.L3:
	RZ_POP
	ret
	ELF_FUNC(__mth_i_dround)
	ELF_SIZE(__mth_i_dround)
