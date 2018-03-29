
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


#ifndef __COS_D_SCALAR_H__
#define __COS_D_SCALAR_H__


#include <assert.h>
#include <stdio.h>
#include <math.h>
#include <immintrin.h>
#include "common_cos.h"

extern "C" double __attribute__ ((noinline)) __fd_cos_1_avx2(double const a);


double static inline
__sin_d_kernel(double const a, int64_t const h)
{
    double s, r, f, t;
    s = a * a;
    r = -A_D;
    r = FMA(r, s, -B_D);
    r = FMA(r, s, -C_D);
    r = FMA(r, s, -D_D);
    r = FMA(r, s, -E_D);
    r = FMA(r, s, -F_D);
    r = FMA(r, s, -G_D);
    f = ll_as_double(double_as_ll(a) ^ h);
    t = s * f;
    r = FMA(r, t, -f);
    return r;
}

double __attribute__ ((noinline))
__fd_cos_1_avx2(double const x)
{

    double a, k, r;
    uint64_t p, h;

    p = double_as_ll(x) & 0x7fffffffffffffffULL;
    if (p <= 0x3e46a09e667f3bccULL)
        return 1.0;
    if (__builtin_expect(p > double_as_ll(THRESHOLD), 0)) {
        a = p >= 0x7ff0000000000000ULL ? x * 0.0 : reduction_slowpath(x, &h);
    } else {
        k = FMA(ll_as_double(p), _1_OVER_PI, -0.5);
        k += 6755399441055744.0;
        h = double_as_ll(k) << 63;
        k -= 6755399441055744.0;
        k = FMA(2.0, k, 1.0);

        a = FMA(k, -PI_2_HI, ll_as_double(p));
        a = FMA(k, -PI_2_MI, a);
        a = FMA(k, -PI_2_LO, a);
    }

    r = __sin_d_kernel(a, h);

    return r;
}

#endif // __COS_D_SCALAR_H__

