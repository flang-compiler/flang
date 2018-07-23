
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

#include <immintrin.h>

/*
 * WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
 *
 * This routine is deprecated and has been replace by directly calling:
 * __fd_asin_8()
 * It should never be defined in the math dispatch configuration tables.
 *
 * WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
 */

extern __m512d __fd_asin_8(__m512d);

__m512d __fvd_asin_evex_512(__m512d const a) {
    return __fd_asin_8(a);
}
