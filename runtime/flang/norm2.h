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

#ifndef _NORM2_H_
#define _NORM2_H_

#include "stdioInterf.h"
#include "fioMacros.h"

#if defined(__AVX2__) && defined(DESC_I8)
#define NORM2_REAL4 norm2_avx2_real4_i8_
#define NORM2_REAL8 norm2_avx2_real8_i8_
#elif defined(__AVX2__) // implies !defined(DESC_I8)
#define NORM2_REAL4 norm2_avx2_real4_
#define NORM2_REAL8 norm2_avx2_real8_
#elif defined(DESC_I8) // implies !defined(__AVX2__)
#define NORM2_REAL4 norm2_real4_i8_
#define NORM2_REAL8 norm2_real8_i8_
#else // implies !defined(__AVX2__) && !defined(DESC_I8)
#define NORM2_REAL4 norm2_real4_
#define NORM2_REAL8 norm2_real8_
#endif

#endif
