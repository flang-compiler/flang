
/* 
 * Copyright (c) 2016-2018, NVIDIA CORPORATION.  All rights reserved.
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

#include <immintrin.h>

extern __m256d __fvd_pow_vex_256(__m256d, __m256d);

__m512d __fvd_pow_evex_512(__m512d const a, __m512d const b) {

    __m256d x, y, z1, z2;
    __m512d res;

    x = _mm512_extractf64x4_pd(a,0);
    y = _mm512_extractf64x4_pd(b,0);
    z1 = __fvd_pow_vex_256(x,y);
    res = _mm512_insertf64x4(res,z1,0);

    x = _mm512_extractf64x4_pd(a,1);
    y = _mm512_extractf64x4_pd(b,1);
    z2 = __fvd_pow_vex_256(x,y);
    res = _mm512_insertf64x4(res,z2,1);

    return res;
}
