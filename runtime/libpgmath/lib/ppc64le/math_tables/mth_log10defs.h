/*
 * Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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

MTHINTRIN(log10, ss   , any        , __mth_i_log10         , __mth_i_log10         , __mth_i_log10         ,__math_dispatch_error)
MTHINTRIN(log10, ds   , any        , __mth_i_dlog10        , __mth_i_dlog10        , __mth_i_dlog10        ,__math_dispatch_error)
MTHINTRIN(log10, sv4  , any        , __gs_log10_4          , __gs_log10_4          , __gs_log10_4          ,__math_dispatch_error)
MTHINTRIN(log10, dv2  , any        , __gd_log10_2          , __gd_log10_2          , __gd_log10_2          ,__math_dispatch_error)
MTHINTRIN(log10, sv4m , any        , __fs_log10_4_mn       , __rs_log10_4_mn       , __ps_log10_4_mn       ,__math_dispatch_error)
MTHINTRIN(log10, dv2m , any        , __fd_log10_2_mn       , __rd_log10_2_mn       , __pd_log10_2_mn       ,__math_dispatch_error)
