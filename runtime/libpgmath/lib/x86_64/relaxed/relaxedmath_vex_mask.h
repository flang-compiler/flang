/*
 * Copyright (c) 2014-2017, NVIDIA CORPORATION.  All rights reserved.
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
 */

/*
 *   __rvs_pow_vex_mask(argument1, argument2, mask)
 *   __rvs_pow_fma4_mask(argument1, argument2, mask)
 *
 *   argument:   xmm0, xmm1
 *   mask:       xmm2
 *
 *   Compute the power of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__rvs_pow_fma4_mask)
ENT(__rvs_pow_fma4_mask):
#else
        .globl ENT(__rvs_pow_vex_mask)
ENT(__rvs_pow_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %xmm2
        je      LBL(.L_rvs_pow_done)

        vandps %xmm0,%xmm2,%xmm0
        vandps %xmm1,%xmm2,%xmm1
#ifdef FMA4_TARGET
        CALL(ENT(__rvs_pow_fma4))
#else
        CALL(ENT(__rvs_pow_vex))
#endif

LBL(.L_rvs_pow_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__rvs_pow_fma4_mask)
        ELF_SIZE(__rvs_pow_fma4_mask)
#else
        ELF_FUNC(__rvs_pow_vex_mask)
        ELF_SIZE(__rvs_pow_vex_mask)
#endif

/*
 *   __rvs_pow_vex_256_mask(argument1, argument2, mask)
 *   __rvs_pow_fma4_256_mask(argument1, argument2, mask)
 *
 *   argument:   ymm0, ymm1
 *   mask:       ymm2
 *
 *   Compute the power of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__rvs_pow_fma4_256_mask)
ENT(__rvs_pow_fma4_256_mask):
#else
        .globl ENT(__rvs_pow_vex_256_mask)
ENT(__rvs_pow_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %ymm2
        je      LBL(.L_rvs_pow_256_done)

        vandps %ymm0,%ymm2,%ymm0
        vandps %ymm1,%ymm2,%ymm1
#ifdef FMA4_TARGET
        CALL(ENT(__rvs_pow_fma4_256))
#else
        CALL(ENT(__rvs_pow_vex_256))
#endif

LBL(.L_rvs_pow_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__rvs_pow_fma4_256_mask)
        ELF_SIZE(__rvs_pow_fma4_256_mask)
#else
        ELF_FUNC(__rvs_pow_vex_256_mask)
        ELF_SIZE(__rvs_pow_vex_256_mask)
#endif

/*
 *   __rvs_exp_vex_mask(argument, mask)
 *   __rvs_exp_fma4_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the exp of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__rvs_exp_fma4_mask)
ENT(__rvs_exp_fma4_mask):
#else
        .globl ENT(__rvs_exp_vex_mask)
ENT(__rvs_exp_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %xmm1
        je      LBL(.L_rvs_exp_done)

        vandps %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__rvs_exp_fma4))
#else
        CALL(ENT(__rvs_exp_vex))
#endif

LBL(.L_rvs_exp_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__rvs_exp_fma4_mask)
        ELF_SIZE(__rvs_exp_fma4_mask)
#else
        ELF_FUNC(__rvs_exp_vex_mask)
        ELF_SIZE(__rvs_exp_vex_mask)
#endif

/*
 *   __rvs_exp_vex_256_mask(argument, mask)
 *   __rvs_exp_fma4_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the exp of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__rvs_exp_fma4_256_mask)
ENT(__rvs_exp_fma4_256_mask):
#else
        .globl ENT(__rvs_exp_vex_256_mask)
ENT(__rvs_exp_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %ymm1
        je      LBL(.L_rvs_exp_256_done)

        vandps %ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
        CALL(ENT(__rvs_exp_fma4_256))
#else
        CALL(ENT(__rvs_exp_vex_256))
#endif

LBL(.L_rvs_exp_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__rvs_exp_fma4_256_mask)
        ELF_SIZE(__rvs_exp_fma4_256_mask)
#else
        ELF_FUNC(__rvs_exp_vex_256_mask)
        ELF_SIZE(__rvs_exp_vex_256_mask)
#endif

/*
 *   __rvd_exp_vex_mask(argument, mask)
 *   __rvd_exp_fma4_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the exp of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__rvd_exp_fma4_mask)
ENT(__rvd_exp_fma4_mask):
#else
        .globl ENT(__rvd_exp_vex_mask)
ENT(__rvd_exp_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je      LBL(.L_rvd_exp_done)

        vandpd  %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__rvd_exp_fma4))
#else
        CALL(ENT(__rvd_exp_vex))
#endif

LBL(.L_rvd_exp_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__rvd_exp_fma4_mask)
        ELF_SIZE(__rvd_exp_fma4_mask)
#else
        ELF_FUNC(__rvd_exp_vex_mask)
        ELF_SIZE(__rvd_exp_vex_mask)
#endif

/*
 *   __rvd_exp_vex_256_mask(argument, mask)
 *   __rvd_exp_fma4_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the exp of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__rvd_exp_fma4_256_mask)
ENT(__rvd_exp_fma4_256_mask):
#else
        .globl ENT(__rvd_exp_vex_256_mask)
ENT(__rvd_exp_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm1
        je      LBL(.L_rvd_exp_256_done)

        vandpd  %ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
        CALL(ENT(__rvd_exp_fma4_256))
#else
        CALL(ENT(__rvd_exp_vex_256))
#endif

LBL(.L_rvd_exp_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__rvd_exp_fma4_256_mask)
        ELF_SIZE(__rvd_exp_fma4_256_mask)
#else
        ELF_FUNC(__rvd_exp_vex_256_mask)
        ELF_SIZE(__rvd_exp_vex_256_mask)
#endif


/*
 *   __rvs_tan_vex_256_mask(argument, mask)
 *   __rvs_tan_fma4_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the tangent of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__rvs_tan_fma4_256_mask)
ENT(__rvs_tan_fma4_256_mask):
#else
        .globl ENT(__rvs_tan_vex_256_mask)
ENT(__rvs_tan_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm1
        je      LBL(.L_rvs_tan_256_done)

        vandpd  %ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
        CALL(ENT(__rvs_tan_fma4_256))
#else
        CALL(ENT(__rvs_tan_vex_256))
#endif

LBL(.L_rvs_tan_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__rvs_tan_fma4_256_mask)
        ELF_SIZE(__rvs_tan_fma4_256_mask)
#else
        ELF_FUNC(__rvs_tan_vex_256_mask)
        ELF_SIZE(__rvs_tan_vex_256_mask)
#endif


/*
 *   __rvs_tan_vex_mask(argument, mask)
 *   __rvs_tan_fma4_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the tangent of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__rvs_tan_fma4_mask)
ENT(__rvs_tan_fma4_mask):
#else
        .globl ENT(__rvs_tan_vex_mask)
ENT(__rvs_tan_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je      LBL(.L_rvs_tan_done)

        vandpd  %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__rvs_tan_fma4))
#else
        CALL(ENT(__rvs_tan_vex))
#endif

LBL(.L_rvs_tan_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__rvs_tan_fma4_mask)
        ELF_SIZE(__rvs_tan_fma4_mask)
#else
        ELF_FUNC(__rvs_tan_vex_mask)
        ELF_SIZE(__rvs_tan_vex_mask)
#endif

