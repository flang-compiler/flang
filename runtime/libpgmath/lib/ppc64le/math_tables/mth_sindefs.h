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

MTHINTRIN(sin  , ss   , any        ,  __mth_i_sin         ,  __mth_i_sin         , __mth_i_sin           ,__math_dispatch_error)
MTHINTRIN(sin  , ds   , any        ,  __mth_i_dsin        ,  __mth_i_dsin        , __mth_i_dsin          ,__math_dispatch_error)
MTHINTRIN(sin  , sv4  , any        ,  __gs_sin_4          ,  __gs_sin_4          , __gs_sin_4            ,__math_dispatch_error)
MTHINTRIN(sin  , dv2  , any        ,  __gd_sin_2          ,  __gd_sin_2          , __gd_sin_2            ,__math_dispatch_error)
MTHINTRIN(sin  , sv4m , any        , __fs_sin_4_mn         , __rs_sin_4_mn         , __ps_sin_4_mn         ,__math_dispatch_error)
MTHINTRIN(sin  , dv2m , any        , __fd_sin_2_mn         , __rd_sin_2_mn         , __pd_sin_2_mn         ,__math_dispatch_error)
