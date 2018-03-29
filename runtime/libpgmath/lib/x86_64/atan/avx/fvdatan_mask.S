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

.L_datan_zeromask: .quad 0xFFFFFFFFFFFFFFFF
                   .quad 0xFFFFFFFFFFFFFFFF
                   .quad 0xFFFFFFFFFFFFFFFF
                   .quad 0xFFFFFFFFFFFFFFFF
                   .quad 0xFFFFFFFFFFFFFFFF
                   .quad 0xFFFFFFFFFFFFFFFF
                   .quad 0xFFFFFFFFFFFFFFFF
                   .quad 0xFFFFFFFFFFFFFFFF

        .text
        ALN_FUNC
        .globl ENT(__fvd_atan_mask)
ENT(__fvd_atan_mask):

        subq $8, %rsp

        vptest  .L_datan_zeromask(%rip), %xmm1
        je      .L_fvd_atan_done

        vandpd  %xmm0,%xmm1,%xmm0
        CALL(ENT(__fvd_atan))

.L_fvd_atan_done:

        addq $8, %rsp
        ret

        ELF_FUNC(__fvd_atan_mask)
        ELF_SIZE(__fvd_atan_mask)


        .text
        ALN_FUNC
        .globl ENT(__fvd_atan_256_mask)
ENT(__fvd_atan_256_mask):

        subq $8, %rsp

        vptest  .L_datan_zeromask(%rip), %ymm1
        je      .L_fvd_atan_256_done

        vandpd  %ymm0,%ymm1,%ymm0
        CALL(ENT(__fvd_atan_256))

.L_fvd_atan_256_done:

        addq $8, %rsp
        ret

        ELF_FUNC(__fvd_atan_256_mask)
        ELF_SIZE(__fvd_atan_256_mask)

