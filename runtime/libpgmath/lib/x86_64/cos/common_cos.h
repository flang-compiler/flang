
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


#ifndef COMMON_H_H63T0LSL
#define COMMON_H_H63T0LSL

#include <stdint.h>

#include <stdio.h>
#include <assert.h>

#define FMA __builtin_fma

/* Constants for Cody-Waite argument reduction */
#define _1_OVER_PI   3.1830988618379069e-01
#define PI_2_HI      (3.1415926535897931e+00/2)
#define PI_2_MI      (1.2246467991473515e-16/2)
#define PI_2_LO      (1.6956855320737797e-31/2)
#define THRESHOLD    2.1474836480000000e+09

/* Coefficents of approximate -sine on [-PI/2,+PI/2] */
#define A_D -7.3733444756922455e-13
#define B_D  1.6048095399332415e-10
#define C_D -2.5051880273889443e-08
#define D_D  2.7557316600532680e-06
#define E_D -1.9841269825055348e-04
#define F_D  8.3333333332855294e-03
#define G_D -1.6666666666666186e-01

/* 1152 bits of 1/PI for Payne-Hanek argument reduction. */
static uint64_t i1opi_f [] = {
    0x35fdafd88fc6ae84ULL,
    0x9e839cfbc5294975ULL,
    0xba93dd63f5f2f8bdULL,
    0xa7a31fb34f2ff516ULL,
    0xb69b3f6793e584dbULL,
    0xf79788c5ad05368fULL,
    0x8ffc4bffef02cc07ULL,
    0x4e422fc5defc941dULL,
    0x9cc8eb1cc1a99cfaULL,
    0x74ce38135a2fbf20ULL,
    0x74411afa975da242ULL,
    0x7f0ef58e5894d39fULL,
    0x0324977504e8c90eULL,
    0xdb92371d2126e970ULL,
    0xff28b1d5ef5de2b0ULL,
    0x6db14acc9e21c820ULL,
    0xfe13abe8fa9a6ee0ULL,
    0x517cc1b727220a94ULL,
    0ULL,
};

/* -fno-strict-aliasing */
static int64_t
double_as_ll(double f)
{
    return *(int64_t*)&f;
}

/* -fno-strict-aliasing */
static double
ll_as_double(int64_t i)
{
    return *(double*)&i;
}

/* Payne-Hanek style argument reduction. */
static double
reduction_slowpath(double const a, uint64_t *h)
{
    uint64_t result[4];
    uint64_t ia = double_as_ll(a);
    uint64_t s = ia & 0x8000000000000000ULL;
    uint64_t e = ((ia >> 52) & 0x7ff) - 1022;
    int32_t idx = 15 - (e >> 6);
    int32_t q;
    ia = ((ia << 11) | 0x8000000000000000ULL) >> 1;

    __uint128_t acc = 0;
    for (q = idx; q < idx + 4; q++) {
        acc += (__uint128_t)ia * i1opi_f[q];
        result[q - idx] = (uint64_t)acc;
        acc >>= 64;
    }

    e = e & 63;
    uint64_t p = result[3];
    if (e) {
        p         = (p << e)         | (result[2] >> (64 - e));
        result[2] = (result[2] << e) | (result[1] >> (64 - e));
    }

    *h = p & 0x8000000000000000ULL;
    uint64_t shi = 0x3c20000000000000ULL;

    p &= 0x7fffffffffffffffULL;
    /* subtract 0.5 */
    p = (int64_t)p - 0x4000000000000000LL;
    if ((int64_t)p < 0) {
        *h ^= 0x8000000000000000ULL;
        p = ~p;
        result[2] = ~result[2];
    }

    int lz = __builtin_clzll(p);
    p = p << lz | result[2] >> (64 - lz);
    shi -= (uint64_t)lz << 52;
    __uint128_t prod = p * (__uint128_t)0xc90fdaa22168c235ULL;
    uint64_t lhi = prod >> 64;
    double r = ll_as_double(shi) * lhi;

    return r;
}

#endif
