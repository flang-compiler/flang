
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

extern __m512 __fvs_log_evex_512(__m512);

__m512 __fvs_log_evex_512_mask(__m512 x, __m512i y) {

    __m512i const ONE = _mm512_set1_epi32(0xffffffff);
  
/*    if (_mm512_testz_si512(y,ONE) == 0) { */
       x = (__m512) _mm512_and_si512((__m512i) x,ONE);
       return( __fvs_log_evex_512(x));
/*    } */
}
