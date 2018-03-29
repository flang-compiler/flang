/* 
 * Copyright (c) 2011-2018, NVIDIA CORPORATION.  All rights reserved.
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



	.text
	ALN_FUNC
#ifdef FMA4_TARGET
	.globl ENT(__fvs_mod_fma4)
ENT(__fvs_mod_fma4):
#else
	.globl ENT(__fvs_mod_vex)
ENT(__fvs_mod_vex):
#endif
        RZ_PUSH

        /* Move all data to memory, then 1st piece to fp stack */
        vmovaps  %xmm1, RZ_OFF(40)(%rsp)
        vmovaps  %xmm0, RZ_OFF(24)(%rsp)
        flds      RZ_OFF(40)(%rsp)
        flds      RZ_OFF(24)(%rsp)

        /* Loop over partial remainder until done */
LBL(.L_remlps1):
        fprem
        fstsw     %ax
        test      $4, %ah
        jnz       LBL(.L_remlps1)

        /* Store result back to memory */
        fstps     RZ_OFF(24)(%rsp)
        fstp      %st(0)

        /* 2 */
        flds      RZ_OFF(36)(%rsp)
        flds      RZ_OFF(20)(%rsp)

        /* Loop over partial remainder until done */
LBL(.L_remlps2):
        fprem
        fstsw     %ax
        test      $4, %ah
        jnz       LBL(.L_remlps2)

        /* Store result back to memory */
        fstps     RZ_OFF(20)(%rsp)
        fstp      %st(0)

        /* 3 */
        flds      RZ_OFF(32)(%rsp)
        flds      RZ_OFF(16)(%rsp)

        /* Loop over partial remainder until done */
LBL(.L_remlps3):
        fprem
        fstsw     %ax
        test      $4, %ah
        jnz       LBL(.L_remlps3)

        /* Store result back to memory */
        fstps     RZ_OFF(16)(%rsp)
        fstp      %st(0)

        /* 4 */
        flds      RZ_OFF(28)(%rsp)
        flds      RZ_OFF(12)(%rsp)

        /* Loop over partial remainder until done */
LBL(.L_remlps4):
        fprem
        fstsw     %ax
        test      $4, %ah
        jnz       LBL(.L_remlps4)

        /* Store result back to memory */
        fstps     RZ_OFF(12)(%rsp)
        fstp      %st(0)

        /* Store back to xmm0 */
        vmovaps    RZ_OFF(24)(%rsp), %xmm0
        RZ_POP
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_mod_fma4)
        ELF_SIZE(__fvs_mod_fma4)
#else
        ELF_FUNC(__fvs_mod_vex)
        ELF_SIZE(__fvs_mod_vex)
#endif

/* ========================================================================= */

	.text
	ALN_FUNC
#ifdef FMA4_TARGET
	.globl ENT(__fvd_mod_fma4)
ENT(__fvd_mod_fma4):
#else
	.globl ENT(__fvd_mod_vex)
ENT(__fvd_mod_vex):
#endif
        RZ_PUSH

        /* Move all data to memory, then 1st piece to fp stack */
        vmovapd  %xmm1, RZ_OFF(40)(%rsp)
        vmovapd  %xmm0, RZ_OFF(24)(%rsp)
        fldl      RZ_OFF(40)(%rsp)
        fldl      RZ_OFF(24)(%rsp)

        /* Loop over partial remainder until done */
LBL(.L_remlpd1):
        fprem
        fstsw     %ax
        test      $4, %ah
        jnz       LBL(.L_remlpd1)

        /* Store result back to memory */
        fstpl     RZ_OFF(24)(%rsp)
        fstp      %st(0)

        fldl      RZ_OFF(32)(%rsp)
        fldl      RZ_OFF(16)(%rsp)

        /* Loop over partial remainder until done */
LBL(.L_remlpd2):
        fprem
        fstsw     %ax
        test      $4, %ah
        jnz       LBL(.L_remlpd2)

        /* Store result back to memory, then xmm0 */
        fstpl     RZ_OFF(16)(%rsp)
        fstp      %st(0)
        vmovapd    RZ_OFF(24)(%rsp), %xmm0

        RZ_POP
	ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_mod_fma4)
        ELF_SIZE(__fvd_mod_fma4)
#else
        ELF_FUNC(__fvd_mod_vex)
        ELF_SIZE(__fvd_mod_vex)
#endif

/* ========================================================================= */

	.text
        ALN_FUNC
#ifdef FMA4_TARGET
	.globl ENT(__fsd_mod_fma4)
ENT(__fsd_mod_fma4):
#else
	.globl ENT(__fsd_mod_vex)
ENT(__fsd_mod_vex):
#endif
	RZ_PUSH

        /* Move arguments to fp stack */
        vmovsd     %xmm1, RZ_OFF(24)(%rsp)
        vmovsd     %xmm0, RZ_OFF(16)(%rsp)
        fldl      RZ_OFF(24)(%rsp)
        fldl      RZ_OFF(16)(%rsp)

        /* Loop over partial remainder until done */
LBL(.L_remlpd):
        fprem
        fstsw     %ax
        test      $4, %ah
        jnz       LBL(.L_remlpd)

        /* Store result back to xmm0 */
        fstpl     RZ_OFF(16)(%rsp)
        fstp      %st(0)
        vmovsd     RZ_OFF(16)(%rsp), %xmm0
        RZ_POP
        ret

#ifdef FMA4_TARGET
	ELF_FUNC(__fsd_mod_fma4)
	ELF_SIZE(__fsd_mod_fma4)
#else
	ELF_FUNC(__fsd_mod_vex)
	ELF_SIZE(__fsd_mod_vex)
#endif

/* ========================================================================= */

	.text
        ALN_FUNC
#ifdef FMA4_TARGET
	.globl ENT(__fss_mod_fma4)
ENT(__fss_mod_fma4):
#else
	.globl ENT(__fss_mod_vex)
ENT(__fss_mod_vex):
#endif
	RZ_PUSH

        /* Move arguments to fp stack */
        vmovss     %xmm1, RZ_OFF(12)(%rsp)
        vmovss     %xmm0, RZ_OFF(8)(%rsp)
        flds      RZ_OFF(12)(%rsp)
        flds      RZ_OFF(8)(%rsp)

        /* Loop over partial remainder until done */
LBL(.L_remlps):
        fprem
        fstsw	%ax
        test	$4, %ah
        jnz	LBL(.L_remlps)

        /* Store result back to xmm0 */
        fstps	RZ_OFF(8)(%rsp)
        fstp	%st(0)
        vmovss	RZ_OFF(8)(%rsp), %xmm0
        RZ_POP
        ret

#ifdef FMA4_TARGET
	ELF_FUNC(__fss_mod_fma4)
	ELF_SIZE(__fss_mod_fma4)
#else
	ELF_FUNC(__fss_mod_vex)
	ELF_SIZE(__fss_mod_vex)
#endif

/* ------------------------------------------------------------------------- */
/* 
 *  vector sinle precision mod
 *
 *  Prototype:
 *
 *      single __fvs_mod_vex/fma4_256(float *x);
 *
 */

        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_mod_fma4_256)
ENT(__fvs_mod_fma4_256):
#else
        .globl ENT(__fvs_mod_vex_256)
ENT(__fvs_mod_vex_256):
#endif

        pushq   %rbp
        movq    %rsp, %rbp
        subq    $128, %rsp

        vmovups %ymm0, 32(%rsp)
        vmovups %ymm1, 96(%rsp)
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_mod_fma4))
#else
        CALL(ENT(__fvs_mod_vex))
#endif

        vmovups         32(%rsp), %ymm2
        vmovups         96(%rsp), %ymm4
        vmovaps         %xmm0, %xmm3
        vextractf128    $1, %ymm2, %xmm2
        vextractf128    $1, %ymm4, %xmm4
        vmovaps         %xmm2, %xmm0
        vmovaps         %xmm4, %xmm1
        vmovups         %ymm3, 64(%rsp)

#ifdef FMA4_TARGET
        CALL(ENT(__fvs_mod_fma4))
#else
        CALL(ENT(__fvs_mod_vex))
#endif
        vmovups 64(%rsp), %ymm1
        vinsertf128     $1, %xmm0, %ymm1, %ymm0

        movq    %rbp, %rsp
        popq    %rbp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_mod_fma4_256)
        ELF_SIZE(__fvs_mod_fma4_256)
#else
        ELF_FUNC(__fvs_mod_vex_256)
        ELF_SIZE(__fvs_mod_vex_256)
#endif


/* ------------------------------------------------------------------------- */
/* 
 *  vector double precision mod
 * 
 *  Prototype:
 * 
 *      double __fvd_mod_vex/fma4_256(double *x);
 * 
 */

        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_mod_fma4_256)
ENT(__fvd_mod_fma4_256):
#else
        .globl ENT(__fvd_mod_vex_256)
ENT(__fvd_mod_vex_256):
#endif

        pushq   %rbp
        movq    %rsp, %rbp
        subq    $128, %rsp

        vmovups %ymm0, 32(%rsp)
        vmovups %ymm1, 96(%rsp)
#ifdef FMA4_TARGET
        CALL(ENT(__fvd_mod_fma4))
#else
        CALL(ENT(__fvd_mod_vex))
#endif

        vmovups         32(%rsp), %ymm2
        vmovups         96(%rsp), %ymm4
        vmovaps         %xmm0, %xmm3
        vextractf128    $1, %ymm2, %xmm2
        vextractf128    $1, %ymm4, %xmm4
        vmovaps         %xmm2, %xmm0
        vmovaps         %xmm4, %xmm1
        vmovups         %ymm3, 64(%rsp)

#ifdef FMA4_TARGET
        CALL(ENT(__fvd_mod_fma4))
#else
        CALL(ENT(__fvd_mod_vex))
#endif
        vmovups 64(%rsp), %ymm1
        vinsertf128     $1, %xmm0, %ymm1, %ymm0

        movq    %rbp, %rsp
        popq    %rbp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_mod_fma4_256)
        ELF_SIZE(__fvd_mod_fma4_256)
#else
        ELF_FUNC(__fvd_mod_vex_256)
        ELF_SIZE(__fvd_mod_vex_256)
#endif
