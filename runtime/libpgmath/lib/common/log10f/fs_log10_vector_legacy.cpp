
/*
 * Copyright (c) 2019, NVIDIA CORPORATION.  All rights reserved.
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


#if !(defined _CPU)
#error: please define _CPU - specific suffix to a function name
#endif

#if !(defined _VL)
#error: please define _VL - Number of elements per vector register
#endif


#include <immintrin.h>
#define CONFIG 1
#if ((_VL) == (4))
#include "helperavx2_128.h"
#elif ((_VL) == (8))
#include "helperavx2.h"
#elif ((_VL) == (16))
#include "helperavx512f.h"
#endif


#define _JOIN4(a,b,c,d) a##b##c##d
#define JOIN4(a,b,c,d) _JOIN4(a,b,c,d)

#define log10_vec JOIN4(__fs_log10_,_VL,_,_CPU)

extern "C" vfloat log10_vec(vfloat const);

#include <log10_vec_legacy.h>
