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
        .globl ENT(__fvs_atan_256)
ENT(__fvs_atan_256):

        subq    $72, %rsp

        vmovups %ymm0, (%rsp)

        CALL(ENT(__fvs_atan))

        vmovups (%rsp), %ymm2
        vmovaps %xmm0, %xmm1
        vextractf128    $1, %ymm2, %xmm0
        vmovups %ymm1, 32(%rsp)

        CALL(ENT(__fvs_atan))
        vmovups 32(%rsp), %ymm1
        vinsertf128     $1, %xmm0, %ymm1, %ymm0

        addq    $72, %rsp
        ret

        ELF_FUNC(__fvs_atan_256)
        ELF_SIZE(__fvs_atan_256)

