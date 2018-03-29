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

        .text
        ALN_FUNC
        .globl ENT(__fvs_acos)
ENT(__fvs_acos):

        subq    $40, %rsp

        vmovupd  %xmm0, (%rsp)                 /* Save xmm0 */

        CALL(ENT(__mth_i_acos))                /* acos(x(1)) */
        vmovss   %xmm0, 16(%rsp)               /* Save first result */

        vmovss 4(%rsp),%xmm0                   /* Fetch x(2) */
        CALL(ENT(__mth_i_acos))                /* acos(x(2)) */
        vmovss   %xmm0, 20(%rsp)               /* Save second result */
        vmovss 8(%rsp),%xmm0                   /* Fetch x(3) */
        CALL(ENT(__mth_i_acos))                /* acos(x(3)) */
        vmovss   %xmm0, 24(%rsp)               /* Save third result */

        vmovss 12(%rsp),%xmm0                  /* Fetch x(4) */
        CALL(ENT(__mth_i_acos))                /* acos(x(4)) */
        vmovss   %xmm0, 28(%rsp)               /* Save fourth result */

        vmovupd  16(%rsp), %xmm0               /* Put all results in xmm0 */

        addq    $40, %rsp
        ret

        ELF_FUNC(__fvs_acos)
        ELF_SIZE(__fvs_acos)

