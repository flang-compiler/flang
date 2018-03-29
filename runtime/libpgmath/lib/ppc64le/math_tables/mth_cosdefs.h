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

MTHINTRIN(cos  , ss   , any        ,  __mth_i_cos         ,  __mth_i_cos         , __mth_i_cos           ,__math_dispatch_error)
MTHINTRIN(cos  , ds   , any        ,  __mth_i_dcos        ,  __mth_i_dcos        , __mth_i_dcos          ,__math_dispatch_error)
MTHINTRIN(cos  , sv4  , any        ,  __gs_cos_4          ,  __gs_cos_4          , __gs_cos_4            ,__math_dispatch_error)
MTHINTRIN(cos  , dv2  , any        ,  __gd_cos_2          ,  __gd_cos_2          , __gd_cos_2            ,__math_dispatch_error)
MTHINTRIN(cos  , sv4m , any        , __fs_cos_4_mn         , __rs_cos_4_mn         , __ps_cos_4_mn         ,__math_dispatch_error)
MTHINTRIN(cos  , dv2m , any        , __fd_cos_2_mn         , __rd_cos_2_mn         , __pd_cos_2_mn         ,__math_dispatch_error)
