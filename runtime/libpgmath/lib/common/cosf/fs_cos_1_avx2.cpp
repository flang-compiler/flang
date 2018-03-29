
/*
 * Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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


#ifndef __COS_F_SCALAR_H__
#define __COS_F_SCALAR_H__


#include <assert.h>
#include <stdio.h>
#include <math.h>
#include <immintrin.h>
#include "common_cosf.h"

extern "C" float __attribute__ ((noinline)) __fs_cos_1_avx2(float const a);

/* Payne-Hanek style argument reduction. */
static float
reduction_slowpath(float const a, int32_t *h)
{
    uint2 m;
    uint32_t ia = float_as_int(a);
    uint32_t result[7];
    uint32_t hi, lo;
    uint32_t e;
    int32_t idx;
    int32_t q;
    e = ((ia >> 23) & 0xff) - 127;
    ia = (ia << 8) | 0x80000000;

    /* compute x * 1/pi */
    idx = 4 - ((e >> 5) & 3);

    hi = 0;
    for (q = 0; q < 6; q++) {
        m = umad32wide(i1opi_f[q], ia, hi);
        lo = m.x;
        hi = m.y;
        result[q] = lo;
    }
    result[q] = hi;

    e = e & 31;
    /* shift result such that hi:lo<63:63> is the least significant
       integer bit, and hi:lo<62:0> are the fractional bits of the result
    */

    uint64_t p = ((uint64_t)result[idx + 2] << 32) | result[idx + 1];

    if (e) {
        q = 32 - e;
        p = (p << e) | (result[idx] >> q);
    }

    /* fraction */
    *h = (result[idx + 2] << e) & 0x80000000;
    p &= 0x7fffffffffffffffULL;
    /* subtract 0.5 */
    p = (int64_t)p - 0x4000000000000000LL;

    double d = (double)(int64_t)p;
    d *= PI_2_M63;
    float r = (float)d;

    return r;
}

float __attribute__ ((noinline))
__fs_cos_1_avx2(float x)
{

    float p, k, r, s, t;
    int h = 0;
    p = int_as_float(float_as_int(x) & 0x7fffffff);
    if (float_as_int(p) <= 0x39800000)
        return 1.0f;
    if (float_as_int(p) > float_as_int(THRESHOLD_F)) {
        x = float_as_int(p) >= 0x7f800000 ? x * 0.0f : reduction_slowpath(x, &h);
    } else {
        k = FMAF(p, _1_OVER_PI_F, -0.5f);
        k += 12582912.0f;
        h = float_as_int(k) << 31;
        k -= 12582912.0f;
        k = FMAF(2.0f, k, 1.0f);

        x = FMAF(k, -PI_2_HI_F, p);
        x = FMAF(k, -PI_2_MI_F, x);
        x = FMAF(k, -PI_2_LO_F, x);
    }
    s = x * x;
    r = A_F;
    r = FMAF(r, s, B_F);
    r = FMAF(r, s, C_F);
    r = FMAF(r, s, D_F);
    x = int_as_float(float_as_int(x) ^ h);
    t = s * x;
    r = FMAF(r, t, -x);

    return r;
}


#endif // __COS_F_SCALAR_H__

