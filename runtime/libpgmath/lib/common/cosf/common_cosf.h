
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

#define FMAF __builtin_fmaf

/* Constants for Cody-Waite argument reduction */
#define _1_OVER_PI_F 3.18309870e-01f
#define PI_2_HI_F    1.57079315e+00f
#define PI_2_MI_F    3.17493937e-06f
#define PI_2_LO_F    6.22298369e-14f
#define THRESHOLD_F  1.00000000e+04f

/* Coefficents of approximate -sine on [-PI/2,+PI/2] */
#define A_F         -2.58840782e-06f
#define B_F          1.98019770e-04f
#define C_F         -8.33297707e-03f
#define D_F          1.66666582e-01f

/* 192 bits of 1/PI for Payne-Hanek argument reduction. */
static uint32_t i1opi_f [] = {
    0x9e21c820,
    0x6db14acc,
    0xfa9a6ee0,
    0xfe13abe8,
    0x27220a94,
    0x517cc1b7,
};

#define PI_2_M63 3.406121580086555e-19

/* -fno-strict-aliasing */
static int32_t
float_as_int(float f)
{
    return *(int32_t*)&f;
}

/* -fno-strict-aliasing */
static float
int_as_float(int32_t i)
{
    return *(float*)&i;
}

typedef struct {
    uint32_t x;
    uint32_t y;
} uint2;

/* -fno-strict-aliasing */
static uint2
umad32wide(uint32_t a, uint32_t b, uint32_t c)
{
    union {
        uint2 ui2;
        uint64_t ull;
    } res;
    res.ull = (uint64_t)a * b + c;
    return res.ui2;
}

#endif
