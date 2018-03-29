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

/*
 * ============================================================
 */

#include "directives.h"

	.text
        ALN_FUNC
	.globl ENT(__mth_i_remainder)
ENT(__mth_i_remainder):
	RZ_PUSH

        /* Move arguments to fp stack */
        movss     %xmm1, RZ_OFF(12)(%rsp)
        movss     %xmm0, RZ_OFF(8)(%rsp)
        flds      RZ_OFF(12)(%rsp)
        flds      RZ_OFF(8)(%rsp)

        /* Loop over partial remainder until done */
.L_remlps:
        fprem1
        fstsw     %ax
        test      $4, %ah
        jnz       .L_remlps

        /* Store result back to xmm0 */
        fstps     RZ_OFF(8)(%rsp)
        fstp      %st(0)
        movss     RZ_OFF(8)(%rsp), %xmm0
        RZ_POP
        ret

	ELF_FUNC(__mth_i_remainder)
	ELF_SIZE(__mth_i_remainder)
