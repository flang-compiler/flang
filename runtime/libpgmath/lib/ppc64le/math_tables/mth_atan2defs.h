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

MTHINTRIN(atan2, ss   , any        , __mth_i_atan2         , __mth_i_atan2         , __mth_i_atan2         ,__math_dispatch_error)
MTHINTRIN(atan2, ds   , any        , __mth_i_datan2        , __mth_i_datan2        , __mth_i_datan2        ,__math_dispatch_error)
MTHINTRIN(atan2, sv4  , any        , __gs_atan2_4          , __gs_atan2_4          , __gs_atan2_4          ,__math_dispatch_error)
MTHINTRIN(atan2, dv2  , any        , __gd_atan2_2          , __gd_atan2_2          , __gd_atan2_2          ,__math_dispatch_error)
MTHINTRIN(atan2, sv4m , any        , __fs_atan2_4_mn       , __rs_atan2_4_mn       , __ps_atan2_4_mn       ,__math_dispatch_error)
MTHINTRIN(atan2, dv2m , any        , __fd_atan2_2_mn       , __rd_atan2_2_mn       , __pd_atan2_2_mn       ,__math_dispatch_error)
