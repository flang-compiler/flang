/* 
 * Copyright (c) 2016-2018, NVIDIA CORPORATION.  All rights reserved.
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


#include "directives.h"

        ALN_QUAD
.L_dpow_zeromask:
        .quad 0xFFFFFFFFFFFFFFFF
        .quad 0xFFFFFFFFFFFFFFFF
        .quad 0xFFFFFFFFFFFFFFFF
        .quad 0xFFFFFFFFFFFFFFFF
.L_dpow_mask_two:
        .quad 0x4000000000000000
        .quad 0x4000000000000000
        .quad 0x4000000000000000
        .quad 0x4000000000000000



        .text
        ALN_FUNC
        .globl ENT(__fvd_pow_fma3_mask)
ENT(__fvd_pow_fma3_mask):

        subq $8, %rsp

        vptest  .L_dpow_zeromask(%rip), %xmm2
        je      .L_fvd_pow_fma3_done

        vmovupd .L_dpow_mask_two(%rip),%xmm3
        vblendvpd %xmm2,%xmm0,%xmm3,%xmm0
        vblendvpd %xmm2,%xmm1,%xmm3,%xmm1

        CALL(ENT(__fvd_pow_fma3))

.L_fvd_pow_fma3_done:

        addq $8, %rsp
        ret

        ELF_FUNC(__fvd_pow_fma3_mask)
        ELF_SIZE(__fvd_pow_fma3_mask)


        .text
        ALN_FUNC
        .globl ENT(__fvd_pow_fma3_256_mask)
ENT(__fvd_pow_fma3_256_mask):

        subq $8, %rsp

        vptest  .L_dpow_zeromask(%rip), %ymm2
        je      .L_fvd_pow_fma3_256_done

        vmovupd .L_dpow_mask_two(%rip),%ymm3
        vblendvpd %ymm2,%ymm0,%ymm3,%ymm0
        vblendvpd %ymm2,%ymm1,%ymm3,%ymm1

        CALL(ENT(__fvd_pow_fma3_256))

.L_fvd_pow_fma3_256_done:

        addq $8, %rsp
        ret

        ELF_FUNC(__fvd_pow_fma3_256_mask)
        ELF_SIZE(__fvd_pow_fma3_256_mask)

