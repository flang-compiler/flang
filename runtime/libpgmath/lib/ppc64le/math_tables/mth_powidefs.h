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

/* R(:)**I4 */
MTHINTRIN(powi1, ss   , any        , __mth_i_rpowi         , __mth_i_rpowi         , __pmth_i_rpowi        ,__math_dispatch_error)
MTHINTRIN(powi1, ds   , any        , __mth_i_dpowi         , __mth_i_dpowi         , __pmth_i_dpowi        ,__math_dispatch_error)
MTHINTRIN(powi1, sv4  , any        , __fx_powi1_4          , __fx_powi1_4          , __px_powi1_4          ,__math_dispatch_error)
MTHINTRIN(powi1, dv2  , any        , __fx_powi1_2          , __fx_powi1_2          , __px_powi1_2          ,__math_dispatch_error)
MTHINTRIN(powi1, sv4m , any        , __fs_powi1_4_mn       , __rs_powi1_4_mn       , __ps_powi1_4_mn       ,__math_dispatch_error)
MTHINTRIN(powi1, dv2m , any        , __fd_powi1_2_mn       , __rd_powi1_2_mn       , __pd_powi1_2_mn       ,__math_dispatch_error)
/* R(:)**I4(:) */
MTHINTRIN(powi , ss   , any        , __mth_i_rpowi         , __mth_i_rpowi         , __pmth_i_rpowi        ,__math_dispatch_error)
MTHINTRIN(powi , ds   , any        , __mth_i_dpowi         , __mth_i_dpowi         , __pmth_i_dpowi        ,__math_dispatch_error)
MTHINTRIN(powi , sv4  , any        , __gs_powi_4           , __gs_powi_4           , __px_powi_4           ,__math_dispatch_error)
MTHINTRIN(powi , dv2  , any        , __gd_powi_2           , __gd_powi_2           , __px_powi_2           ,__math_dispatch_error)
MTHINTRIN(powi , sv4m , any        , __fs_powi_4_mn        , __rs_powi_4_mn        , __ps_powi_4_mn        ,__math_dispatch_error)
MTHINTRIN(powi , dv2m , any        , __fd_powi_2_mn        , __rd_powi_2_mn        , __pd_powi_2_mn        ,__math_dispatch_error)
/* R(:)**I8 */
MTHINTRIN(powk1, ss   , any        , __mth_i_rpowk         , __mth_i_rpowk         , __pmth_i_rpowk        ,__math_dispatch_error)
MTHINTRIN(powk1, ds   , any        , __mth_i_dpowk         , __mth_i_dpowk         , __pmth_i_dpowk        ,__math_dispatch_error)
MTHINTRIN(powk1, sv4  , any        , __fx_powk1_4          , __fx_powk1_4          , __px_powk1_4          ,__math_dispatch_error)
MTHINTRIN(powk1, dv2  , any        , __fx_powk1_2          , __fx_powk1_2          , __px_powk1_2          ,__math_dispatch_error)
MTHINTRIN(powk1, sv4m , any        , __fs_powk1_4_mn       , __rs_powk1_4_mn       , __ps_powk1_4_mn       ,__math_dispatch_error)
MTHINTRIN(powk1, dv2m , any        , __fd_powk1_2_mn       , __rd_powk1_2_mn       , __pd_powk1_2_mn       ,__math_dispatch_error)
/* R(:)**I8(:) */
MTHINTRIN(powk , ss   , any        , __mth_i_rpowk         , __mth_i_rpowk         , __pmth_i_rpowk        ,__math_dispatch_error)
MTHINTRIN(powk , ds   , any        , __mth_i_dpowk         , __mth_i_dpowk         , __pmth_i_dpowk        ,__math_dispatch_error)
MTHINTRIN(powk , sv4  , any        , __gs_powk_4           , __gs_powk_4           , __px_powk_4           ,__math_dispatch_error)
MTHINTRIN(powk , dv2  , any        , __gd_powk_2           , __gd_powk_2           , __px_powk_2           ,__math_dispatch_error)
MTHINTRIN(powk , sv4m , any        , __fs_powk_4_mn        , __rs_powk_4_mn        , __ps_powk_4_mn        ,__math_dispatch_error)
MTHINTRIN(powk , dv2m , any        , __fd_powk_2_mn        , __rd_powk_2_mn        , __pd_powk_2_mn        ,__math_dispatch_error)

