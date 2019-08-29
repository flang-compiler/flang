
/*
 * Copyright (c) 2019, NVIDIA CORPORATION.  All rights reserved.
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

#include <common.h>
#include <immintrin.h>
#include <math.h>


#if !(defined _CPU)
#error: please define _CPU - specific suffix to a function name
#endif

#define _JOIN2(a,b) a##b
#define JOIN2(a,b) _JOIN2(a,b)

#define atan_scalar JOIN2(__fs_atan_1_,_CPU)
#define FMAF __builtin_fmaf

extern "C" float atan_scalar(float);


float __attribute__((noinline)) atan_scalar(const float x) {

    bool xBig = (fabsf(x) > 1.0f);

    float xReduced = x;

    if (xBig){
        xReduced = 1.0f / x;
    }

    float x2 = xReduced*xReduced;
    
    // We evaluate the polynomial using the Horner scheme:
    float x4 = x2 * x2;
    float x8 = x4 * x4;

    // First layer of Estrin:
    float L1 = FMAF(x2, C2, C1);
    float L2 = FMAF(x2, C4, C3);
    float L3 = FMAF(x2, C6, C5);
    float L4 = FMAF(x2, C8, C7);


    // We now want:
    // L1 + x4*L2 + x8*L3 + x12*L4 =
    //(L1 + x4*L2) + x8*(L3 + x4*L4)
    // Second layer of estrin
    float M1 = FMAF(x4, L2, L1);
    float M2 = FMAF(x4, L4, L3);

    float poly = FMAF(x8, M2, M1);


    if (xBig) {
        const float signedPi = copysignf(PI_2, x);

        float result_d = FMAF(-x2 * xReduced, poly, (signedPi - xReduced));
        return result_d;
    }

    float result_d = FMAF(x2 * xReduced, poly, xReduced);

    return result_d;
}
