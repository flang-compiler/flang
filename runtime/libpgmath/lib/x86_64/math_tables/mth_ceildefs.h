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

MTHINTRIN(ceil  , ss   , any        ,  __mth_i_ceil         ,  __mth_i_ceil         , __mth_i_ceil          ,__math_dispatch_error)
MTHINTRIN(ceil  , ds   , any        ,  __mth_i_dceil        , __mth_i_dceil         , __mth_i_dceil         ,__math_dispatch_error)
MTHINTRIN(ceil  , sv4  , any        ,  __gs_ceil_4_f        ,  __gs_ceil_4_r        , __gs_ceil_4_p         ,__math_dispatch_error)
MTHINTRIN(ceil  , dv2  , any        ,  __gd_ceil_2_f        ,  __gd_ceil_2_r        , __gd_ceil_2_p         ,__math_dispatch_error)
MTHINTRIN(ceil  , sv8  , any        ,  __gs_ceil_8_f        ,  __gs_ceil_8_r        , __gs_ceil_8_p         ,__math_dispatch_error)
MTHINTRIN(ceil  , dv4  , any        ,  __gd_ceil_4_f        ,  __gd_ceil_4_r        , __gd_ceil_4_p         ,__math_dispatch_error)
MTHINTRIN(ceil  , sv16 , any        ,  __gs_ceil_16_f       ,  __gs_ceil_16_r       , __gs_ceil_16_p        ,__math_dispatch_error)
MTHINTRIN(ceil  , dv8  , any        ,  __gd_ceil_8_f        ,  __gd_ceil_8_r        , __gd_ceil_8_p         ,__math_dispatch_error)
MTHINTRIN(ceil  , sv4m , any        , __fs_ceil_4_mn        , __rs_ceil_4_mn        , __ps_ceil_4_mn        ,__math_dispatch_error)
MTHINTRIN(ceil  , dv2m , any        , __fd_ceil_2_mn        , __rd_ceil_2_mn        , __pd_ceil_2_mn        ,__math_dispatch_error)
MTHINTRIN(ceil  , sv8m , any        , __fs_ceil_8_mn        , __rs_ceil_8_mn        , __ps_ceil_8_mn        ,__math_dispatch_error)
MTHINTRIN(ceil  , dv4m , any        , __fd_ceil_4_mn        , __rd_ceil_4_mn        , __pd_ceil_4_mn        ,__math_dispatch_error)
MTHINTRIN(ceil  , sv16m, any        , __fs_ceil_16_mn       , __rs_ceil_16_mn       , __ps_ceil_16_mn       ,__math_dispatch_error)
MTHINTRIN(ceil  , dv8m , any        , __fd_ceil_8_mn        , __rd_ceil_8_mn        , __pd_ceil_8_mn        ,__math_dispatch_error)
