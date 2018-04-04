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

MTHINTRIN(atan2, ss   , em64t      , __mth_i_atan2         , __mth_i_atan2         , __mth_i_atan2         ,__math_dispatch_error)
MTHINTRIN(atan2, ds   , em64t      , __mth_i_datan2        , __mth_i_datan2        , __mth_i_datan2        ,__math_dispatch_error)
MTHINTRIN(atan2, sv4  , em64t      , __gs_atan2_4          , __gs_atan2_4          , __gs_atan2_4          ,__math_dispatch_error)
MTHINTRIN(atan2, dv2  , em64t      , __gd_atan2_2          , __gd_atan2_2          , __gd_atan2_2          ,__math_dispatch_error)
MTHINTRIN(atan2, sv4m , em64t      , __fs_atan2_4_mn       , __rs_atan2_4_mn       , __ps_atan2_4_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, dv2m , em64t      , __fd_atan2_2_mn       , __rd_atan2_2_mn       , __pd_atan2_2_mn       ,__math_dispatch_error)

MTHINTRIN(atan2, ss   , sse4       , __mth_i_atan2         , __mth_i_atan2         , __mth_i_atan2         ,__math_dispatch_error)
MTHINTRIN(atan2, ds   , sse4       , __mth_i_datan2        , __mth_i_datan2        , __mth_i_datan2        ,__math_dispatch_error)
MTHINTRIN(atan2, sv4  , sse4       , __gs_atan2_4          , __gs_atan2_4          , __gs_atan2_4          ,__math_dispatch_error)
MTHINTRIN(atan2, dv2  , sse4       , __gd_atan2_2          , __gd_atan2_2          , __gd_atan2_2          ,__math_dispatch_error)
MTHINTRIN(atan2, sv4m , sse4       , __fs_atan2_4_mn       , __rs_atan2_4_mn       , __ps_atan2_4_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, dv2m , sse4       , __fd_atan2_2_mn       , __rd_atan2_2_mn       , __pd_atan2_2_mn       ,__math_dispatch_error)

MTHINTRIN(atan2, ss   , avx        , __mth_i_atan2         , __mth_i_atan2         , __mth_i_atan2         ,__math_dispatch_error)
MTHINTRIN(atan2, ds   , avx        , __mth_i_datan2        , __mth_i_datan2        , __mth_i_datan2        ,__math_dispatch_error)
MTHINTRIN(atan2, sv4  , avx        , __gs_atan2_4          , __gs_atan2_4          , __gs_atan2_4          ,__math_dispatch_error)
MTHINTRIN(atan2, dv2  , avx        , __gd_atan2_2          , __gd_atan2_2          , __gd_atan2_2          ,__math_dispatch_error)
MTHINTRIN(atan2, sv8  , avx        , __gs_atan2_8          , __gs_atan2_8          , __gs_atan2_8          ,__math_dispatch_error)
MTHINTRIN(atan2, dv4  , avx        , __gd_atan2_4          , __gd_atan2_4          , __gd_atan2_4          ,__math_dispatch_error)
MTHINTRIN(atan2, sv4m , avx        , __fs_atan2_4_mn       , __rs_atan2_4_mn       , __ps_atan2_4_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, dv2m , avx        , __fd_atan2_2_mn       , __rd_atan2_2_mn       , __pd_atan2_2_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, sv8m , avx        , __fs_atan2_8_mn       , __rs_atan2_8_mn       , __ps_atan2_8_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, dv4m , avx        , __fd_atan2_4_mn       , __rd_atan2_4_mn       , __pd_atan2_4_mn       ,__math_dispatch_error)


MTHINTRIN(atan2, ss   , avxfma4    , __mth_i_atan2         , __mth_i_atan2         , __mth_i_atan2         ,__math_dispatch_error)
MTHINTRIN(atan2, ds   , avxfma4    , __mth_i_datan2        , __mth_i_datan2        , __mth_i_datan2        ,__math_dispatch_error)
MTHINTRIN(atan2, sv4  , avxfma4    , __gs_atan2_4          , __gs_atan2_4          , __gs_atan2_4          ,__math_dispatch_error)
MTHINTRIN(atan2, dv2  , avxfma4    , __gd_atan2_2          , __gd_atan2_2          , __gd_atan2_2          ,__math_dispatch_error)
MTHINTRIN(atan2, sv8  , avxfma4    , __gs_atan2_8          , __gs_atan2_8          , __gs_atan2_8          ,__math_dispatch_error)
MTHINTRIN(atan2, dv4  , avxfma4    , __gd_atan2_4          , __gd_atan2_4          , __gd_atan2_4          ,__math_dispatch_error)
MTHINTRIN(atan2, sv4m , avxfma4    , __fs_atan2_4_mn       , __rs_atan2_4_mn       , __ps_atan2_4_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, dv2m , avxfma4    , __fd_atan2_2_mn       , __rd_atan2_2_mn       , __pd_atan2_2_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, sv8m , avxfma4    , __fs_atan2_8_mn       , __rs_atan2_8_mn       , __ps_atan2_8_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, dv4m , avxfma4    , __fd_atan2_4_mn       , __rd_atan2_4_mn       , __pd_atan2_4_mn       ,__math_dispatch_error)


MTHINTRIN(atan2, ss   , avx2       , __mth_i_atan2         , __mth_i_atan2         , __mth_i_atan2         ,__math_dispatch_error)
MTHINTRIN(atan2, ds   , avx2       , __mth_i_datan2        , __mth_i_datan2        , __mth_i_datan2        ,__math_dispatch_error)
MTHINTRIN(atan2, sv4  , avx2       , __gs_atan2_4          , __gs_atan2_4          , __gs_atan2_4          ,__math_dispatch_error)
MTHINTRIN(atan2, dv2  , avx2       , __gd_atan2_2          , __gd_atan2_2          , __gd_atan2_2          ,__math_dispatch_error)
MTHINTRIN(atan2, sv8  , avx2       , __gs_atan2_8          , __gs_atan2_8          , __gs_atan2_8          ,__math_dispatch_error)
MTHINTRIN(atan2, dv4  , avx2       , __gd_atan2_4          , __gd_atan2_4          , __gd_atan2_4          ,__math_dispatch_error)
MTHINTRIN(atan2, sv4m , avx2       , __fs_atan2_4_mn       , __rs_atan2_4_mn       , __ps_atan2_4_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, dv2m , avx2       , __fd_atan2_2_mn       , __rd_atan2_2_mn       , __pd_atan2_2_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, sv8m , avx2       , __fs_atan2_8_mn       , __rs_atan2_8_mn       , __ps_atan2_8_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, dv4m , avx2       , __fd_atan2_4_mn       , __rd_atan2_4_mn       , __pd_atan2_4_mn       ,__math_dispatch_error)

MTHINTRIN(atan2, ss   , avx512knl  , __mth_i_atan2         , __mth_i_atan2         , __mth_i_atan2         ,__math_dispatch_error)
MTHINTRIN(atan2, ds   , avx512knl  , __mth_i_datan2        , __mth_i_datan2        , __mth_i_datan2        ,__math_dispatch_error)
MTHINTRIN(atan2, sv4  , avx512knl  , __gs_atan2_4          , __gs_atan2_4          , __gs_atan2_4          ,__math_dispatch_error)
MTHINTRIN(atan2, dv2  , avx512knl  , __gd_atan2_2          , __gd_atan2_2          , __gd_atan2_2          ,__math_dispatch_error)
MTHINTRIN(atan2, sv8  , avx512knl  , __gs_atan2_8          , __gs_atan2_8          , __gs_atan2_8          ,__math_dispatch_error)
MTHINTRIN(atan2, dv4  , avx512knl  , __gd_atan2_4          , __gd_atan2_4          , __gd_atan2_4          ,__math_dispatch_error)
MTHINTRIN(atan2, sv16 , avx512knl  , __gs_atan2_16         , __gs_atan2_16         , __gs_atan2_16         ,__math_dispatch_error)
MTHINTRIN(atan2, dv8  , avx512knl  , __gd_atan2_8          , __gd_atan2_8          , __gd_atan2_8          ,__math_dispatch_error)
MTHINTRIN(atan2, sv4m , avx512knl  , __fs_atan2_4_mn       , __rs_atan2_4_mn       , __ps_atan2_4_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, dv2m , avx512knl  , __fd_atan2_2_mn       , __rd_atan2_2_mn       , __pd_atan2_2_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, sv8m , avx512knl  , __fs_atan2_8_mn       , __rs_atan2_8_mn       , __ps_atan2_8_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, dv4m , avx512knl  , __fd_atan2_4_mn       , __rd_atan2_4_mn       , __pd_atan2_4_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, sv16m, avx512knl  , __fs_atan2_16_mn      , __rs_atan2_16_mn      , __ps_atan2_16_mn      ,__math_dispatch_error)
MTHINTRIN(atan2, dv8m , avx512knl  , __fd_atan2_8_mn       , __rd_atan2_8_mn       , __pd_atan2_8_mn       ,__math_dispatch_error)

MTHINTRIN(atan2, ss   , avx512     , __mth_i_atan2         , __mth_i_atan2         , __mth_i_atan2         ,__math_dispatch_error)
MTHINTRIN(atan2, ds   , avx512     , __mth_i_datan2        , __mth_i_datan2        , __mth_i_datan2        ,__math_dispatch_error)
MTHINTRIN(atan2, sv4  , avx512     , __gs_atan2_4          , __gs_atan2_4          , __gs_atan2_4          ,__math_dispatch_error)
MTHINTRIN(atan2, dv2  , avx512     , __gd_atan2_2          , __gd_atan2_2          , __gd_atan2_2          ,__math_dispatch_error)
MTHINTRIN(atan2, sv8  , avx512     , __gs_atan2_8          , __gs_atan2_8          , __gs_atan2_8          ,__math_dispatch_error)
MTHINTRIN(atan2, dv4  , avx512     , __gd_atan2_4          , __gd_atan2_4          , __gd_atan2_4          ,__math_dispatch_error)
MTHINTRIN(atan2, sv16 , avx512     , __gs_atan2_16         , __gs_atan2_16         , __gs_atan2_16         ,__math_dispatch_error)
MTHINTRIN(atan2, dv8  , avx512     , __gd_atan2_8          , __gd_atan2_8          , __gd_atan2_8          ,__math_dispatch_error)
MTHINTRIN(atan2, sv4m , avx512     , __fs_atan2_4_mn       , __rs_atan2_4_mn       , __ps_atan2_4_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, dv2m , avx512     , __fd_atan2_2_mn       , __rd_atan2_2_mn       , __pd_atan2_2_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, sv8m , avx512     , __fs_atan2_8_mn       , __rs_atan2_8_mn       , __ps_atan2_8_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, dv4m , avx512     , __fd_atan2_4_mn       , __rd_atan2_4_mn       , __pd_atan2_4_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, sv16m, avx512     , __fs_atan2_16_mn      , __rs_atan2_16_mn      , __ps_atan2_16_mn      ,__math_dispatch_error)
MTHINTRIN(atan2, dv8m , avx512     , __fd_atan2_8_mn       , __rd_atan2_8_mn       , __pd_atan2_8_mn       ,__math_dispatch_error)
