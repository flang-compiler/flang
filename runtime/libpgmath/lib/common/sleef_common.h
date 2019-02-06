
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

#if !(defined __SLEEF_COMMON_H_INCLUDED__)
#define __SLEEF_COMMON_H_INCLUDED__ 1

// SLEEF helpers
#define CONFIG 1
#if (defined __SLEEF_AVX2_128__)
    #include "helperavx2_128.h"
    #define __SIMD_TYPE _mm
    #define __SIMD_BITS 128
#elif (defined __SLEEF_AVX2_256__)
    #include "helperavx2.h"
    #define __SIMD_TYPE _mm256
    #define __SIMD_BITS 256
#elif (defined __SLEEF_AVX512__)
    #include "helperavx512f.h"
    #define __SIMD_TYPE _mm512
    #define __SIMD_BITS 512
#endif

#define vF2I vreinterpret_vi2_vf
#define vI2F vreinterpret_vf_vi2
#define vD2I vreinterpret_vi2_vd
#define vD2L vD2I
#define vL2D vreinterpret_vd_vi2
#define vD2F(x) vI2F(vD2I(x))
#define vSETi vcast_vi2_i
#define vSETf vcast_vf_f
#define vSETd vcast_vd_d
#define vSETLLi(x, y) vD2I(vSETd(L2D(II2L(x, y))))
#define vSETll(x) vD2L(vSETd(L2D(x)))
#define vSETLLL(x, y) vD2L(vset_vd_d_d(L2D(x), L2D(y)))

static void INLINE
vfast2sum(vfloat x, vfloat y, vfloat *rhi, vfloat *rlo)
{
    vfloat tmp, hi, lo;
    hi  = vadd_vf_vf_vf(x, y);
    tmp = vsub_vf_vf_vf(hi, x);
    lo  = vsub_vf_vf_vf(y, tmp);
    *rhi = hi;
    *rlo = lo;
    return;
}

static void INLINE
vfast2mul(vfloat x, vfloat y, vfloat *r1, vfloat *r2)
{
    vfloat p1 = vmul_vf_vf_vf(x, y);
    vfloat p2 = vfmapn_vf_vf_vf_vf(x, y, p1);
    *r1 = p1;
    *r2 = p2;
}

static void INLINE
vfast2sum_dp(vdouble x, vdouble y, vdouble *rhi, vdouble *rlo)
{
    vdouble tmp, hi, lo;
    hi  = vadd_vd_vd_vd(x, y);
    tmp = vsub_vd_vd_vd(hi, x);
    lo  = vsub_vd_vd_vd(y, tmp);
    *rhi = hi;
    *rlo = lo;
    return;
}

static void INLINE
vfast2mul_dp(vdouble x, vdouble y, vdouble *r1, vdouble *r2)
{
    vdouble p1 = vmul_vd_vd_vd(x, y);
    vdouble p2 = vfmapn_vd_vd_vd_vd(x, y, p1);
    *r1 = p1;
    *r2 = p2;
}

#endif //!(defined __SLEEF_COMMON_H_INCLUDED__)
