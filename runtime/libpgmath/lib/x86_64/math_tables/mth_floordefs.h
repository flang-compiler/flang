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

MTHINTRIN(floor  , ss   , any        ,  __mth_i_floor         ,  __mth_i_floor         , __mth_i_floor          ,__math_dispatch_error)
MTHINTRIN(floor  , ds   , any        ,  __mth_i_dfloor        ,  __mth_i_dfloor        , __mth_i_dfloor         ,__math_dispatch_error)
MTHINTRIN(floor  , sv4  , any        ,  __gs_floor_4_f        ,  __gs_floor_4_r        , __gs_floor_4_p         ,__math_dispatch_error)
MTHINTRIN(floor  , dv2  , any        ,  __gd_floor_2_f        ,  __gd_floor_2_r        , __gd_floor_2_p         ,__math_dispatch_error)
MTHINTRIN(floor  , sv8  , any        ,  __gs_floor_8_f        ,  __gs_floor_8_r        , __gs_floor_8_p         ,__math_dispatch_error)
MTHINTRIN(floor  , dv4  , any        ,  __gd_floor_4_f        ,  __gd_floor_4_r        , __gd_floor_4_p         ,__math_dispatch_error)
MTHINTRIN(floor  , sv16 , any        ,  __gs_floor_16_f       ,  __gs_floor_16_r       , __gs_floor_16_p        ,__math_dispatch_error)
MTHINTRIN(floor  , dv8  , any        ,  __gd_floor_8_f        ,  __gd_floor_8_r        , __gd_floor_8_p         ,__math_dispatch_error)
MTHINTRIN(floor  , sv4m , any        , __fs_floor_4_mn        , __rs_floor_4_mn        , __ps_floor_4_mn        ,__math_dispatch_error)
MTHINTRIN(floor  , dv2m , any        , __fd_floor_2_mn        , __rd_floor_2_mn        , __pd_floor_2_mn        ,__math_dispatch_error)
MTHINTRIN(floor  , sv8m , any        , __fs_floor_8_mn        , __rs_floor_8_mn        , __ps_floor_8_mn        ,__math_dispatch_error)
MTHINTRIN(floor  , dv4m , any        , __fd_floor_4_mn        , __rd_floor_4_mn        , __pd_floor_4_mn        ,__math_dispatch_error)
MTHINTRIN(floor  , sv16m, any        , __fs_floor_16_mn       , __rs_floor_16_mn       , __ps_floor_16_mn       ,__math_dispatch_error)
MTHINTRIN(floor  , dv8m , any        , __fd_floor_8_mn        , __rd_floor_8_mn        , __pd_floor_8_mn        ,__math_dispatch_error)
