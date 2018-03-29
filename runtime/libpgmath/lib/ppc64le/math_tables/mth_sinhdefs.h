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

MTHINTRIN(sinh , ss   , any        , __mth_i_sinh          , __mth_i_sinh          , __mth_i_sinh          ,__math_dispatch_error)
MTHINTRIN(sinh , ds   , any        , __mth_i_dsinh         , __mth_i_dsinh         , __mth_i_dsinh         ,__math_dispatch_error)
MTHINTRIN(sinh , sv4  , any        , __gs_sinh_4           , __gs_sinh_4           , __gs_sinh_4           ,__math_dispatch_error)
MTHINTRIN(sinh , dv2  , any        , __gd_sinh_2           , __gd_sinh_2           , __gd_sinh_2           ,__math_dispatch_error)
MTHINTRIN(sinh , sv4m , any        , __fs_sinh_4_mn        , __rs_sinh_4_mn        , __ps_sinh_4_mn        ,__math_dispatch_error)
MTHINTRIN(sinh , dv2m , any        , __fd_sinh_2_mn        , __rd_sinh_2_mn        , __pd_sinh_2_mn        ,__math_dispatch_error)
