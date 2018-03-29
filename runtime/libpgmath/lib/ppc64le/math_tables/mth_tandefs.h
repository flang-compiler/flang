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

MTHINTRIN(tan  , ss   , any        , __mth_i_tan          , __mth_i_tan          , __mth_i_tan           ,__math_dispatch_error)
MTHINTRIN(tan  , ds   , any        , __mth_i_dtan         , __mth_i_dtan         , __mth_i_dtan          ,__math_dispatch_error)
MTHINTRIN(tan  , sv4  , any        , __gs_tan_4           , __gs_tan_4           , __gs_tan_4            ,__math_dispatch_error)
MTHINTRIN(tan  , dv2  , any        , __gd_tan_2           , __gd_tan_2           , __gd_tan_2            ,__math_dispatch_error)
MTHINTRIN(tan  , sv4m , any        , __fs_tan_4_mn         , __rs_tan_4_mn         , __ps_tan_4_mn         ,__math_dispatch_error)
MTHINTRIN(tan  , dv2m , any        , __fd_tan_2_mn         , __rd_tan_2_mn         , __pd_tan_2_mn         ,__math_dispatch_error)
