
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

extern __m256d __fvd_exp_vex_256(__m256d);

__m512d __fvd_exp_evex_512(__m512d const a) {

    __m256d x, y1, y2;
    __m512d res;

    x = _mm512_extractf64x4_pd(a,0);
    y1 = __fvd_exp_vex_256(x);
    res = _mm512_insertf64x4(res,y1,0);

    x = _mm512_extractf64x4_pd(a,1);
    y2 = __fvd_exp_vex_256(x);
    res = _mm512_insertf64x4(res,y2,1);

    return res;
}
