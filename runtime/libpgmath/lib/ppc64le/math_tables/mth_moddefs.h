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

MTHINTRIN(mod   , ss   , any        , __mth_i_amod           , __mth_i_amod           , __mth_i_amod           ,__math_dispatch_error)
MTHINTRIN(mod   , ds   , any        , __mth_i_dmod           , __mth_i_dmod           , __mth_i_dmod           ,__math_dispatch_error)
MTHINTRIN(mod   , sv4  , any        , __gs_mod_4             , __gs_mod_4             , __gs_mod_4             ,__math_dispatch_error)
MTHINTRIN(mod   , dv2  , any        , __gd_mod_2             , __gd_mod_2             , __gd_mod_2             ,__math_dispatch_error)
MTHINTRIN(mod   , sv4m , any        , __fs_mod_4_mn          , __rs_mod_4_mn          , __ps_mod_4_mn          ,__math_dispatch_error)
MTHINTRIN(mod   , dv2m , any        , __fd_mod_2_mn          , __rd_mod_2_mn          , __pd_mod_2_mn          ,__math_dispatch_error)
