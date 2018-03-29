
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

extern double __attribute__ ((noinline)) __fd_sincos_1_avx2(double const a);
extern double __fd_sin_1_avx2(double);
extern double __fd_cos_1_avx2(double);

double
__fd_sincos_1_avx2(double a)
{
    double s;
    double c;

/*
 *  The cosine function MUST be called first.
 */

    c = __fd_cos_1_avx2(a);
    s = __fd_sin_1_avx2(a);
    asm("vmovsd\t%0,%%xmm1" : : "m"(c) : "%xmm1");
    return s;
}


