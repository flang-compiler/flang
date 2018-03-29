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

#define	SLEEFINTRIN(name)\
MTHINTRIN(name , ss  , sse4       , __mth_slf_##name##f_u10		, __mth_slf_##name##f_u35		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , ds  , sse4       , __mth_slf_##name##_u10		, __mth_slf_##name##_u35		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , sv4 , sse4       , __mth_slf_##name##f4_u10sse4	, __mth_slf_##name##f4_u35sse4		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , dv2 , sse4       , __mth_slf_##name##d2_u10sse4	, __mth_slf_##name##d2_u35sse4		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , ss  , avx        , __mth_slf_##name##f_u10		, __mth_slf_##name##f_u35		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , ds  , avx        , __mth_slf_##name##_u10		, __mth_slf_##name##_u35		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , sv4 , avx        , __mth_slf_##name##f4_u10sse4	, __mth_slf_##name##f4_u35sse4		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , dv2 , avx        , __mth_slf_##name##d2_u10sse4	, __mth_slf_##name##d2_u35sse4		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , sv8 , avx        , __mth_slf_##name##f8_u10avx		, __mth_slf_##name##f8_u35avx		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , dv4 , avx        , __mth_slf_##name##d4_u10avx		, __mth_slf_##name##d4_u35avx		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , ss  , avx2       , __mth_slf_##name##f_u10		, __mth_slf_##name##f_u35		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , ds  , avx2       , __mth_slf_##name##_u10		, __mth_slf_##name##_u35		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , sv4 , avx2       , __mth_slf_##name##f4_u10avx2128	, __mth_slf_##name##f4_u35avx2128	, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , dv2 , avx2       , __mth_slf_##name##d2_u10avx2128	, __mth_slf_##name##d2_u35avx2128	, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , sv8 , avx2       , __mth_slf_##name##f8_u10avx2	, __mth_slf_##name##f8_u35avx2		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , dv4 , avx2       , __mth_slf_##name##d4_u10avx2	, __mth_slf_##name##d4_u35avx2		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , ss  , avx512     , __mth_slf_##name##f_u10		, __mth_slf_##name##f_u35		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , ds  , avx512     , __mth_slf_##name##_u10		, __mth_slf_##name##_u35		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , sv4 , avx512     , __mth_slf_##name##f4_u10avx2128	, __mth_slf_##name##f4_u35avx2128	, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , dv2 , avx512     , __mth_slf_##name##d2_u10avx2128	, __mth_slf_##name##d2_u35avx2128	, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , sv8 , avx512     , __mth_slf_##name##f8_u10avx2	, __mth_slf_##name##f8_u35avx2		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , dv4 , avx512     , __mth_slf_##name##d4_u10avx2	, __mth_slf_##name##d4_u35avx2		, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , sv16, avx512     , __mth_slf_##name##f16_u10avx512f	, __mth_slf_##name##f16_u35avx512f	, __math_dispatch_error, __math_dispatch_error) \
MTHINTRIN(name , dv8 , avx512     , __mth_slf_##name##d8_u10avx512f	, __mth_slf_##name##d8_u35avx512f	, __math_dispatch_error, __math_dispatch_error) \

SLEEFINTRIN(acos)
SLEEFINTRIN(asin)
SLEEFINTRIN(atan)
SLEEFINTRIN(atan2)
SLEEFINTRIN(cos)
SLEEFINTRIN(sin)
SLEEFINTRIN(tan)
SLEEFINTRIN(cosh)
SLEEFINTRIN(sinh)
SLEEFINTRIN(tanh)
SLEEFINTRIN(exp)
SLEEFINTRIN(log)
SLEEFINTRIN(log10)
SLEEFINTRIN(pow)
SLEEFINTRIN(sincos)
