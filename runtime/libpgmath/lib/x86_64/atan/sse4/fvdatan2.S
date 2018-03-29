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
        .globl ENT(__fvdatan)
ENT(__fvdatan):

        subq    $40, %rsp

        movupd  %xmm0, (%rsp)                  /* Save xmm0 */

        CALL(ENT(__mth_i_datan))               /* atan(x(1)) */
        movsd   %xmm0, 16(%rsp)                /* Save first result */

        movsd 8(%rsp),%xmm0                    /* Fetch x(2) */
        CALL(ENT(__mth_i_datan))               /* atan(x(2)) */
        movsd   %xmm0, 24(%rsp)                /* Save second result */

        movupd  16(%rsp), %xmm0                /* Put all results in xmm0 */

        addq    $40, %rsp
        ret

        ELF_FUNC(__fvdatan)
        ELF_SIZE(__fvdatan)








