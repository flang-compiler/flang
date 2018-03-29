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

MTHINTRIN(pow  , ss   , any        ,  __mth_i_rpowr       ,  __mth_i_rpowr       , __mth_i_rpowr         ,__math_dispatch_error)
MTHINTRIN(pow  , ds   , any        ,  __mth_i_dpowd       ,  __mth_i_dpowd       , __mth_i_dpowd         ,__math_dispatch_error)
MTHINTRIN(pow  , sv4  , any        ,  __gs_pow_4          ,  __gs_pow_4          , __gs_pow_4            ,__math_dispatch_error)
MTHINTRIN(pow  , dv2  , any        ,  __gd_pow_2          ,  __gd_pow_2          , __gd_pow_2            ,__math_dispatch_error)
MTHINTRIN(pow  , sv4m , any        , __fs_pow_4_mn         , __rs_pow_4_mn         , __ps_pow_4_mn         ,__math_dispatch_error)
MTHINTRIN(pow  , dv2m , any        , __fd_pow_2_mn         , __rd_pow_2_mn         , __pd_pow_2_mn         ,__math_dispatch_error)
