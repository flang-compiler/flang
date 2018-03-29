
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

#if defined(TARGET_LINUX_POWER)
#include "xmm2altivec.h"
#else
#include <immintrin.h>
#endif
#include "fdlog_defs.h"

extern "C" double __fsd_log_fma3(double);

// casts int to double
inline
__m128d __internal_fast_int2dbl(__m128i a)
{
    __m128i const INT2DBL_HI = _mm_set1_epi64x(INT2DBL_HI_D);
    __m128i const INT2DBL_LO = _mm_set1_epi64x(INT2DBL_LO_D);
    __m128d const INT2DBL    = (__m128d)_mm_set1_epi64x(INT2DBL_D);

    __m128i t = _mm_xor_si128(INT2DBL_LO, a);
    t = _mm_blend_epi32(INT2DBL_HI, t, H55_D); 
    return _mm_sub_sd((__m128d)t, INT2DBL);
}

// special cases for log
__m128d __attribute__ ((noinline)) __pgm_log_d_scalar_special_cases(__m128d const a, __m128d z)
{
    __m128d const ZERO       = _mm_set1_pd(ZERO_D);
    __m128i const ALL_ONES_EXPONENT = _mm_set1_epi64x(ALL_ONES_EXPONENT_D);
    __m128d const NAN_VAL   = (__m128d)_mm_set1_epi64x(NAN_VAL_D);
    __m128d const NEG_INF  = (__m128d)_mm_set1_epi64x(NEG_INF_D);


    __m128i detect_inf_nan = (__m128i)_mm_sub_sd(a, a); 
    __m128d inf_nan_mask = (__m128d)_mm_cmpeq_epi64(_mm_and_si128(detect_inf_nan, ALL_ONES_EXPONENT), ALL_ONES_EXPONENT);
   
    // inf + inf = inf = log(inf). nan + nan = nan = log(nan).
    __m128i inf_nan = (__m128i)_mm_add_sd(a, a);
    z = _mm_blendv_pd(z, (__m128d)inf_nan, inf_nan_mask); 
    
    __m128d non_positive_mask = _mm_cmp_sd(a, ZERO, _CMP_LT_OQ);
    // log(negative number) = NaN
    z = _mm_blendv_pd(z, NAN_VAL, non_positive_mask);

    __m128d zero_mask = _mm_cmp_sd(a, ZERO, _CMP_EQ_OQ);
    z = _mm_blendv_pd(z, NEG_INF, zero_mask);
     
    return z;
}

double __fsd_log_fma3(double const a_in)
{
     __m128d const HI_CONST_1   = (__m128d)_mm_set1_epi64x(HI_CONST_1_D);
     __m128d const HI_CONST_2   = (__m128d)_mm_set1_epi64x(HI_CONST_2_D);
     __m128i const HALFIFIER    = _mm_set1_epi64x(HALFIFIER_D); 
     __m128i const HI_THRESH    = _mm_set1_epi64x(HI_THRESH_D); 
     __m128d const ONE_F        = _mm_set1_pd(ONE_F_D);
     __m128d const ZERO         = _mm_set1_pd(ZERO_D);

     __m128d const LN2_HI       = _mm_set1_pd(LN2_HI_D);
     __m128d const LN2_LO       = _mm_set1_pd(LN2_LO_D);
 
     __m128i const HI_MASK      = _mm_set1_epi64x(HI_MASK_D);
     __m128i const ONE          = _mm_set1_epi64x(ONE_D);

     __m128i const TEN_23      = _mm_set1_epi64x(TEN_23_D);
     __m128i const ALL_ONES_EXPONENT = _mm_set1_epi64x(ALL_ONES_EXPONENT_D);

    __m128d const LOG_C1_VEC = _mm_set1_pd(   LOG_C1_VEC_D    );
    __m128d const LOG_C2_VEC = _mm_set1_pd(   LOG_C2_VEC_D    );
    __m128d const LOG_C3_VEC = _mm_set1_pd(   LOG_C3_VEC_D    );
    __m128d const LOG_C4_VEC = _mm_set1_pd(   LOG_C4_VEC_D    );
    __m128d const LOG_C5_VEC = _mm_set1_pd(   LOG_C5_VEC_D    );
    __m128d const LOG_C6_VEC = _mm_set1_pd(   LOG_C6_VEC_D    );
    __m128d const LOG_C7_VEC = _mm_set1_pd(   LOG_C7_VEC_D    );
    __m128d const LOG_C8_VEC = _mm_set1_pd(   LOG_C8_VEC_D    );
    __m128d const LOG_C9_VEC = _mm_set1_pd(   LOG_C9_VEC_D    );
    __m128d const LOG_C10_VEC = _mm_set1_pd(  LOG_C10_VEC_D   );
    __m128d const LOG_C11_VEC = _mm_set1_pd(  LOG_C11_VEC_D   );
    __m128d const LOG_C12_VEC = _mm_set1_pd(  LOG_C12_VEC_D   );
    __m128d const LOG_C13_VEC = _mm_set1_pd(  LOG_C13_VEC_D   );
    __m128d const LOG_C14_VEC = _mm_set1_pd(  LOG_C14_VEC_D   );
    __m128d const LOG_C15_VEC = _mm_set1_pd(  LOG_C15_VEC_D   );
    __m128d const LOG_C16_VEC = _mm_set1_pd(  LOG_C16_VEC_D   );
    __m128d const LOG_C17_VEC = _mm_set1_pd(  LOG_C17_VEC_D   );
    __m128d const LOG_C18_VEC = _mm_set1_pd(  LOG_C18_VEC_D   );
    __m128d const LOG_C19_VEC = _mm_set1_pd(  LOG_C19_VEC_D   );
    __m128d const LOG_C20_VEC = _mm_set1_pd(  LOG_C20_VEC_D   );
    __m128d const LOG_C21_VEC = _mm_set1_pd(  LOG_C21_VEC_D   );
    __m128d const LOG_C22_VEC = _mm_set1_pd(  LOG_C22_VEC_D   );
    __m128d const LOG_C23_VEC = _mm_set1_pd(  LOG_C23_VEC_D   );
    __m128d const LOG_C24_VEC = _mm_set1_pd(  LOG_C24_VEC_D   );

    __m128d a_mut, m, f;
    __m128i expo, expo_plus1;
    __m128d thresh_mask;
    __m128d a = _mm_set1_pd(a_in);

    // isolate mantissa 
    a_mut = _mm_and_pd(a, HI_CONST_1);
    a_mut = _mm_or_pd(a_mut, HI_CONST_2);

    // magic trick to improve accuracy (divide mantissa by 2 and increase exponent by 1)
    thresh_mask = _mm_cmp_pd(a_mut, (__m128d)HI_THRESH, _CMP_GT_OS);
    m = (__m128d)_mm_sub_epi32((__m128i)a_mut, HALFIFIER);
    m = _mm_blendv_pd(a_mut, m, thresh_mask);

    expo = _mm_srli_epi64((__m128i)a, D52_D);
    expo = _mm_sub_epi64(expo, TEN_23);
    expo_plus1 = _mm_add_epi64(expo, ONE); 
    expo = (__m128i)_mm_blendv_pd((__m128d)expo, (__m128d)expo_plus1, thresh_mask);

    // computing polynomial for log(1+m)
    m = _mm_sub_sd(m, ONE_F);

    // estrin scheme for highest 16 terms, then estrin again for the next 8. Finally finish off with horner.
    __m128d z9  = _mm_fmadd_sd(LOG_C10_VEC, m, LOG_C9_VEC);
    __m128d z11 = _mm_fmadd_sd(LOG_C12_VEC, m, LOG_C11_VEC);
    __m128d z13 = _mm_fmadd_sd(LOG_C14_VEC, m, LOG_C13_VEC);
    __m128d z15 = _mm_fmadd_sd(LOG_C16_VEC, m, LOG_C15_VEC);
    __m128d z17 = _mm_fmadd_sd(LOG_C18_VEC, m, LOG_C17_VEC);
    __m128d z19 = _mm_fmadd_sd(LOG_C20_VEC, m, LOG_C19_VEC);
    __m128d z21 = _mm_fmadd_sd(LOG_C22_VEC, m, LOG_C21_VEC);
    __m128d z23 = _mm_fmadd_sd(LOG_C24_VEC, m, LOG_C23_VEC);

    __m128d m2 = _mm_mul_sd(m, m);
    z9  = _mm_fmadd_sd(z11, m2, z9);
    z13 = _mm_fmadd_sd(z15, m2, z13);
    z17 = _mm_fmadd_sd(z19, m2, z17);
    z21 = _mm_fmadd_sd(z23, m2, z21);

    __m128d m4 = _mm_mul_sd(m2, m2);
    z9  = _mm_fmadd_sd(z13, m4, z9);
    z17 = _mm_fmadd_sd(z21, m4, z17);

    __m128d m8 = _mm_mul_sd(m4, m4);
    z9 = _mm_fmadd_sd(z17, m8, z9);

    // estrin for the next 8 terms
    __m128d z8 = _mm_fmadd_pd(z9, m, LOG_C8_VEC);
    __m128d z6 = _mm_fmadd_pd(LOG_C7_VEC, m, LOG_C6_VEC);
    __m128d z4 = _mm_fmadd_pd(LOG_C5_VEC, m, LOG_C4_VEC);
    __m128d z2 = _mm_fmadd_pd(LOG_C3_VEC, m, LOG_C2_VEC);

    z6 = _mm_fmadd_pd(z8, m2, z6);
    z2 = _mm_fmadd_pd(z4, m2, z2);
    __m128d z = _mm_fmadd_pd(z6, m4, z2);

    // finish computation with horner
    z = _mm_fmadd_sd(z, m, LOG_C1_VEC);
    z = _mm_mul_sd(z, m);

    f = __internal_fast_int2dbl(expo);
    z = _mm_fmadd_sd(f, LN2_HI, z);
     
    // compute special cases (inf, NaN, negative, 0)
    __m128i detect_inf_nan = (__m128i)_mm_sub_sd(a, a);
    __m128i detect_non_positive = (__m128i)_mm_cmp_sd(a, ZERO, _CMP_LE_OQ);
    __m128i overridemask = _mm_cmpeq_epi64(_mm_and_si128(detect_inf_nan, ALL_ONES_EXPONENT), ALL_ONES_EXPONENT);

#if defined(TARGET_LINUX_POWER)
    int specMask = _vec_any_nz((__m128i)_mm_or_si128(detect_non_positive, overridemask));
#else
    int specMask = _mm_movemask_pd((__m128d)_mm_or_si128(detect_non_positive, overridemask));
#endif
    if(__builtin_expect(specMask, 0)) {
        return _mm_cvtsd_f64(__pgm_log_d_scalar_special_cases(a, z));
    }
    return _mm_cvtsd_f64(z);
}
     
