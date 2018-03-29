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

MTHINTRIN(exp  , ss   , any        ,  __mth_i_exp         ,  __mth_i_exp         , __mth_i_exp           ,__math_dispatch_error)
MTHINTRIN(exp  , ds   , any        ,  __mth_i_dexp        ,  __mth_i_dexp        , __mth_i_dexp          ,__math_dispatch_error)
MTHINTRIN(exp  , sv4  , any        ,  __gs_exp_4          ,  __gs_exp_4          , __gs_exp_4            ,__math_dispatch_error)
MTHINTRIN(exp  , dv2  , any        ,  __gd_exp_2          ,  __gd_exp_2          , __gd_exp_2            ,__math_dispatch_error)
MTHINTRIN(exp  , sv4m , any        , __fs_exp_4_mn         , __rs_exp_4_mn         , __ps_exp_4_mn         ,__math_dispatch_error)
MTHINTRIN(exp  , dv2m , any        , __fd_exp_2_mn         , __rd_exp_2_mn         , __pd_exp_2_mn         ,__math_dispatch_error)
