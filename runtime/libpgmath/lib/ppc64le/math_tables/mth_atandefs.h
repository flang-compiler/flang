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

MTHINTRIN(atan , ss   , any        ,  __mth_i_atan        ,  __mth_i_atan        , __mth_i_atan          ,__math_dispatch_error)
MTHINTRIN(atan , ds   , any        ,  __mth_i_datan       ,  __mth_i_datan       , __mth_i_datan         ,__math_dispatch_error)
MTHINTRIN(atan , sv4  , any        ,  __gs_atan_4         ,  __gs_atan_4         , __gs_atan_4           ,__math_dispatch_error)
MTHINTRIN(atan , dv2  , any        ,  __gd_atan_2         ,  __gd_atan_2         , __gd_atan_2           ,__math_dispatch_error)
MTHINTRIN(atan , sv4m , any        , __fs_atan_4_mn        , __rs_atan_4_mn        , __ps_atan_4_mn        ,__math_dispatch_error)
MTHINTRIN(atan , dv2m , any        , __fd_atan_2_mn        , __rd_atan_2_mn        , __pd_atan_2_mn        ,__math_dispatch_error)
