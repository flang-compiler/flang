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

MTHINTRIN(log10, ss   , em64t      , __mth_i_alog10        , __mth_i_alog10        , __mth_i_log10         ,__math_dispatch_error)
MTHINTRIN(log10, ds   , em64t      , __mth_i_dlog10        , __mth_i_dlog10        , __mth_i_dlog10        ,__math_dispatch_error)
MTHINTRIN(log10, sv4  , em64t      , __fvslog10            , __fvslog10            , __gs_log10_4          ,__math_dispatch_error)
MTHINTRIN(log10, dv2  , em64t      , __fvdlog10            , __fvdlog10            , __gd_log10_2          ,__math_dispatch_error)
MTHINTRIN(log10, sv4m , em64t      , __fs_log10_4_mn       , __rs_log10_4_mn       , __ps_log10_4_mn       ,__math_dispatch_error)
MTHINTRIN(log10, dv2m , em64t      , __fd_log10_2_mn       , __rd_log10_2_mn       , __pd_log10_2_mn       ,__math_dispatch_error)

MTHINTRIN(log10, ss   , sse4       , __fss_log10           , __fss_log10           , __mth_i_log10         ,__math_dispatch_error)
MTHINTRIN(log10, ds   , sse4       , __fsd_log10           , __fsd_log10           , __mth_i_dlog10        ,__math_dispatch_error)
MTHINTRIN(log10, sv4  , sse4       , __fvs_log10           , __fvs_log10           , __gs_log10_4          ,__math_dispatch_error)
MTHINTRIN(log10, dv2  , sse4       , __fvd_log10           , __fvd_log10           , __gd_log10_2          ,__math_dispatch_error)
MTHINTRIN(log10, sv4m , sse4       , __fs_log10_4_mn       , __rs_log10_4_mn       , __ps_log10_4_mn       ,__math_dispatch_error)
MTHINTRIN(log10, dv2m , sse4       , __fd_log10_2_mn       , __rd_log10_2_mn       , __pd_log10_2_mn       ,__math_dispatch_error)

MTHINTRIN(log10, ss   , avx        , __fss_log10_vex       , __fss_log10_vex       , __mth_i_log10         ,__math_dispatch_error)
MTHINTRIN(log10, ds   , avx        , __fsd_log10_vex       , __fsd_log10_vex       , __mth_i_dlog10        ,__math_dispatch_error)
MTHINTRIN(log10, sv4  , avx        , __fvs_log10_vex       , __fvs_log10_vex       , __gs_log10_4          ,__math_dispatch_error)
MTHINTRIN(log10, dv2  , avx        , __fvd_log10_vex       , __fvd_log10_vex       , __gd_log10_2          ,__math_dispatch_error)
MTHINTRIN(log10, sv8  , avx        , __fvs_log10_vex_256   , __fvs_log10_vex_256   , __gs_log10_8          ,__math_dispatch_error)
MTHINTRIN(log10, dv4  , avx        , __fvd_log10_vex_256   , __fvd_log10_vex_256   , __gd_log10_4          ,__math_dispatch_error)
MTHINTRIN(log10, sv4m , avx        , __fs_log10_4_mn       , __rs_log10_4_mn       , __ps_log10_4_mn       ,__math_dispatch_error)
MTHINTRIN(log10, dv2m , avx        , __fd_log10_2_mn       , __rd_log10_2_mn       , __pd_log10_2_mn       ,__math_dispatch_error)
MTHINTRIN(log10, sv8m , avx        , __fs_log10_8_mn       , __rs_log10_8_mn       , __ps_log10_8_mn       ,__math_dispatch_error)
MTHINTRIN(log10, dv4m , avx        , __fd_log10_4_mn       , __rd_log10_4_mn       , __pd_log10_4_mn       ,__math_dispatch_error)

MTHINTRIN(log10, ss   , avxfma4    , __fss_log10_fma4      , __fss_log10_fma4      , __mth_i_log10         ,__math_dispatch_error)
MTHINTRIN(log10, ds   , avxfma4    , __fsd_log10_fma4      , __fsd_log10_fma4      , __mth_i_dlog10        ,__math_dispatch_error)
MTHINTRIN(log10, sv4  , avxfma4    , __fvs_log10_fma4      , __fvs_log10_fma4      , __gs_log10_4          ,__math_dispatch_error)
MTHINTRIN(log10, dv2  , avxfma4    , __fvd_log10_fma4      , __fvd_log10_fma4      , __gd_log10_2          ,__math_dispatch_error)
MTHINTRIN(log10, sv8  , avxfma4    , __fvs_log10_fma4_256  , __fvs_log10_fma4_256  , __gs_log10_8          ,__math_dispatch_error)
MTHINTRIN(log10, dv4  , avxfma4    , __fvd_log10_fma4_256  , __fvd_log10_fma4_256  , __gd_log10_4          ,__math_dispatch_error)
MTHINTRIN(log10, sv4m , avxfma4    , __fs_log10_4_mn       , __rs_log10_4_mn       , __ps_log10_4_mn       ,__math_dispatch_error)
MTHINTRIN(log10, dv2m , avxfma4    , __fd_log10_2_mn       , __rd_log10_2_mn       , __pd_log10_2_mn       ,__math_dispatch_error)
MTHINTRIN(log10, sv8m , avxfma4    , __fs_log10_8_mn       , __rs_log10_8_mn       , __ps_log10_8_mn       ,__math_dispatch_error)
MTHINTRIN(log10, dv4m , avxfma4    , __fd_log10_4_mn       , __rd_log10_4_mn       , __pd_log10_4_mn       ,__math_dispatch_error)

MTHINTRIN(log10, ss   , avx2       , __fss_log10_vex       , __fss_log10_vex       , __mth_i_log10         ,__math_dispatch_error)
MTHINTRIN(log10, ds   , avx2       , __fsd_log10_vex       , __fsd_log10_vex       , __mth_i_dlog10        ,__math_dispatch_error)
MTHINTRIN(log10, sv4  , avx2       , __fvs_log10_vex       , __fvs_log10_vex       , __gs_log10_4          ,__math_dispatch_error)
MTHINTRIN(log10, dv2  , avx2       , __fvd_log10_vex       , __fvd_log10_vex       , __gd_log10_2          ,__math_dispatch_error)
MTHINTRIN(log10, sv8  , avx2       , __fvs_log10_vex_256   , __fvs_log10_vex_256   , __gs_log10_8          ,__math_dispatch_error)
MTHINTRIN(log10, dv4  , avx2       , __fvd_log10_vex_256   , __fvd_log10_vex_256   , __gd_log10_4          ,__math_dispatch_error)
MTHINTRIN(log10, sv4m , avx2       , __fs_log10_4_mn       , __rs_log10_4_mn       , __ps_log10_4_mn       ,__math_dispatch_error)
MTHINTRIN(log10, dv2m , avx2       , __fd_log10_2_mn       , __rd_log10_2_mn       , __pd_log10_2_mn       ,__math_dispatch_error)
MTHINTRIN(log10, sv8m , avx2       , __fs_log10_8_mn       , __rs_log10_8_mn       , __ps_log10_8_mn       ,__math_dispatch_error)
MTHINTRIN(log10, dv4m , avx2       , __fd_log10_4_mn       , __rd_log10_4_mn       , __pd_log10_4_mn       ,__math_dispatch_error)

MTHINTRIN(log10, ss   , avx512knl  , __fss_log10_vex       , __fss_log10_vex       , __mth_i_log10         ,__math_dispatch_error)
MTHINTRIN(log10, ds   , avx512knl  , __fsd_log10_vex       , __fsd_log10_vex       , __mth_i_dlog10        ,__math_dispatch_error)
MTHINTRIN(log10, sv4  , avx512knl  , __fvs_log10_vex       , __fvs_log10_vex       , __gs_log10_4          ,__math_dispatch_error)
MTHINTRIN(log10, dv2  , avx512knl  , __fvd_log10_vex       , __fvd_log10_vex       , __gd_log10_2          ,__math_dispatch_error)
MTHINTRIN(log10, sv8  , avx512knl  , __fvs_log10_vex_256   , __fvs_log10_vex_256   , __gs_log10_8          ,__math_dispatch_error)
MTHINTRIN(log10, dv4  , avx512knl  , __fvd_log10_vex_256   , __fvd_log10_vex_256   , __gd_log10_4          ,__math_dispatch_error)
MTHINTRIN(log10, sv16 , avx512knl  , __fs_log10_16_z2yy    , __rs_log10_16_z2yy    , __gs_log10_16         ,__math_dispatch_error)
MTHINTRIN(log10, dv8  , avx512knl  , __fd_log10_8_z2yy     , __rd_log10_8_z2yy     , __gd_log10_8          ,__math_dispatch_error)
MTHINTRIN(log10, sv4m , avx512knl  , __fs_log10_4_mn       , __rs_log10_4_mn       , __ps_log10_4_mn       ,__math_dispatch_error)
MTHINTRIN(log10, dv2m , avx512knl  , __fd_log10_2_mn       , __rd_log10_2_mn       , __pd_log10_2_mn       ,__math_dispatch_error)
MTHINTRIN(log10, sv8m , avx512knl  , __fs_log10_8_mn       , __rs_log10_8_mn       , __ps_log10_8_mn       ,__math_dispatch_error)
MTHINTRIN(log10, dv4m , avx512knl  , __fd_log10_4_mn       , __rd_log10_4_mn       , __pd_log10_4_mn       ,__math_dispatch_error)
MTHINTRIN(log10, sv16m, avx512knl  , __fs_log10_16_mn      , __rs_log10_16_mn      , __ps_log10_16_mn      ,__math_dispatch_error)
MTHINTRIN(log10, dv8m , avx512knl  , __fd_log10_8_mn       , __rd_log10_8_mn       , __pd_log10_8_mn       ,__math_dispatch_error)

MTHINTRIN(log10, ss   , avx512     , __fss_log10_vex       , __fss_log10_vex       , __mth_i_log10         ,__math_dispatch_error)
MTHINTRIN(log10, ds   , avx512     , __fsd_log10_vex       , __fsd_log10_vex       , __mth_i_dlog10        ,__math_dispatch_error)
MTHINTRIN(log10, sv4  , avx512     , __fvs_log10_vex       , __fvs_log10_vex       , __gs_log10_4          ,__math_dispatch_error)
MTHINTRIN(log10, dv2  , avx512     , __fvd_log10_vex       , __fvd_log10_vex       , __gd_log10_2          ,__math_dispatch_error)
MTHINTRIN(log10, sv8  , avx512     , __fvs_log10_vex_256   , __fvs_log10_vex_256   , __gs_log10_8          ,__math_dispatch_error)
MTHINTRIN(log10, dv4  , avx512     , __fvd_log10_vex_256   , __fvd_log10_vex_256   , __gd_log10_4          ,__math_dispatch_error)
MTHINTRIN(log10, sv16 , avx512     , __fs_log10_16_z2yy    , __rs_log10_16_z2yy    , __gs_log10_16         ,__math_dispatch_error)
MTHINTRIN(log10, dv8  , avx512     , __fd_log10_8_z2yy     , __rd_log10_8_z2yy     , __gd_log10_8          ,__math_dispatch_error)
MTHINTRIN(log10, sv4m , avx512     , __fs_log10_4_mn       , __rs_log10_4_mn       , __ps_log10_4_mn       ,__math_dispatch_error)
MTHINTRIN(log10, dv2m , avx512     , __fd_log10_2_mn       , __rd_log10_2_mn       , __pd_log10_2_mn       ,__math_dispatch_error)
MTHINTRIN(log10, sv8m , avx512     , __fs_log10_8_mn       , __rs_log10_8_mn       , __ps_log10_8_mn       ,__math_dispatch_error)
MTHINTRIN(log10, dv4m , avx512     , __fd_log10_4_mn       , __rd_log10_4_mn       , __pd_log10_4_mn       ,__math_dispatch_error)
MTHINTRIN(log10, sv16m, avx512     , __fs_log10_16_mn      , __rs_log10_16_mn      , __ps_log10_16_mn      ,__math_dispatch_error)
MTHINTRIN(log10, dv8m , avx512     , __fd_log10_8_mn       , __rd_log10_8_mn       , __pd_log10_8_mn       ,__math_dispatch_error)
