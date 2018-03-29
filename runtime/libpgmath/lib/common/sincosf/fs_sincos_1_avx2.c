/*
 * Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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

#include <math.h>

extern float __attribute__ ((noinline)) __fs_sincos_1_avx2(float const
 a);
extern float __fs_sin_1_avx2(float);
extern float __fs_cos_1_avx2(float);


float
__fs_sincos_1_avx2(float d)
{
    float s;
    float c;

/*
 *  The cosine function MUST be called first.
 */

    c = __fs_cos_1_avx2(d);
    s = __fs_sin_1_avx2(d);
    asm("vmovss\t%0,%%xmm1" : : "m"(c) : "%xmm1");
    return s;
}
