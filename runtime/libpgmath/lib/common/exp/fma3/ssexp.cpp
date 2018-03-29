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
#include "exp_defs.h"

#define FMAF __builtin_fmaf

extern "C" float __fss_exp_fma3(float);

inline float itf(int a)
{
    return *reinterpret_cast<float*>(&a);
}

inline int fti(float a)
{
    return *reinterpret_cast<int*>(&a);
}

float __fss_exp_fma3(float a)
{
    if (a != a)
        return a;
    if (a >= EXP_HI)
        return itf(INF);
    if (a <= EXP_LO)
        return 0.0f;
    float t = FMAF(a, L2E, FLT2INT_CVT);
    float tt = t - FLT2INT_CVT;
    float z = FMAF(tt, -LN2_0, a);
          z = FMAF(tt, -LN2_1, z);

    int exp = fti(t);
        exp <<= 23;

    float zz =             EXP_C7;
          zz = FMAF(zz, z, EXP_C6);
          zz = FMAF(zz, z, EXP_C5);
          zz = FMAF(zz, z, EXP_C4);
          zz = FMAF(zz, z, EXP_C3);
          zz = FMAF(zz, z, EXP_C2);
          zz = FMAF(zz, z, EXP_C1);
          zz = FMAF(zz, z, EXP_C0);

    if (a <= EXP_DN) {
        int dnrm = exp > DNRM_THR ? DNRM_THR : exp;
        dnrm = dnrm + DNRM_SHFT;
        exp = exp > DNRM_THR ? exp : DNRM_THR;
        float res = itf(exp + fti(zz));
        res = res * itf(dnrm);
        return res;
    } else {
        return itf(exp + fti(zz));
    }
}
