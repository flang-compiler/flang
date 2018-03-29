
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

#ifndef __SIN_D_AVX512_H__
#define __SIN_D_AVX512_H__

#include <assert.h>
#include <immintrin.h>
#define CONFIG 1
#include "helperavx512f.h"
#ifndef TARGET_OSX_X8664
#include "common_sin.h"
#include "sin_d_vec.h"
#endif

extern "C" vdouble __attribute__ ((noinline)) __fd_sin_8_avx512(vdouble const a);

vdouble __attribute__ ((noinline))
__fd_sin_8_avx512(vdouble const a)
{
#ifndef TARGET_OSX_X8664
	return __sin_d_vec(a);
#else
        assert(0);
        return ((vdouble) _mm512_set1_epi32(0));
#endif
}

#endif // __SIN_D_AVX512_H__
