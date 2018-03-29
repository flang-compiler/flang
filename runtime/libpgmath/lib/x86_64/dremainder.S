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
	.globl ENT(__mth_i_dremainder)
ENT(__mth_i_dremainder):
	RZ_PUSH

        /* Move arguments to fp stack */
        movsd     %xmm1, RZ_OFF(24)(%rsp)
        movsd     %xmm0, RZ_OFF(16)(%rsp)
        fldl      RZ_OFF(24)(%rsp)
        fldl      RZ_OFF(16)(%rsp)

        /* Loop over partial remainder until done */
.L_remlpd:
        fprem1
        fstsw     %ax
        test      $4, %ah
        jnz       .L_remlpd

        /* Store result back to xmm0 */
        fstpl     RZ_OFF(16)(%rsp)
        fstp      %st(0)
        movsd     RZ_OFF(16)(%rsp), %xmm0
        RZ_POP
        ret

	ELF_FUNC(__mth_i_dremainder)
	ELF_SIZE(__mth_i_dremainder)
